{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}

{-|
Module      : Network.N2O.Internal
Description : Basic types and core functions
Copyright   : (c) Marat Khafizov, 2018
License     : BSD-3
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

Basic types and core functions

-}
module Network.N2O.Internal
 ( module Network.N2O.Internal
 , module Control.Monad.Trans.Reader
 ) where
import qualified Data.Serialize as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.IORef
import Data.Map.Strict (Map, (!?), insert)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (forM_)
import Control.Concurrent (ThreadId)

-- | An HTTP header
type Header = (BS.ByteString, BS.ByteString)

-- | An HTTP request
data Req = Req
  { reqPath :: BS.ByteString
  , reqMeth :: BS.ByteString
  , reqVers :: BS.ByteString
  , reqHead :: [Header]
  }

-- | The N2O context data type
-- This is the key data type of the N2O. @(f :: * -> *)@ - type constructor
-- for the protocol handler's input type. @(a :: *)@ - base type for the
-- event handler's input type. I.e. @(f a)@ gives input type for the
-- protocol handler. @(Event a)@ gives input type for the event handler.
data Context (f :: * -> *) a where
 Context ::
  { cxHandler :: Event a -> N2O f a (Result a)
  , cxReq :: Req
  , cxMiddleware :: [Context f a -> Context f a]
  , cxProtos :: [Proto f a]
  , cxActions :: TVar BS.ByteString
  , cxDict :: TVar (M.Map BS.ByteString BS.ByteString)
  , cxPubSub :: TVar (M.Map BS.ByteString [TChan (f a)])
  , cxInBox :: TChan (f a)
  , cxOutBox :: TChan (f a)
  , cxSessions :: TVar (M.Map BS.ByteString BS.ByteString)
  , cxTid :: ThreadId
  } -> Context f a

-- | Result of the message processing
data Result a
  = Reply a
  | Ok
  | Unknown
  | Empty
  deriving (Show, Eq)

-- | N2O protocol handler
type Proto (f :: * -> *) a = (f a) -> N2O f a (Result (f a))

-- | Event data type
data Event a
  = Init
  | Message a
  | Terminate
  deriving Show

type N2O f a = ReaderT (Context f a) IO

sub :: BS.ByteString -> N2O f a ()
sub topic = do
  cx <- ask
  liftIO $ atomically $ do
    Context{cxPubSub = pubsub,cxOutBox = chan} <- pure cx
    modifyTVar pubsub $ \m -> M.alter (\mbs -> let s = case mbs of {Just s -> s; _ -> []} in Just $ ins chan s) topic m

unsub topic = do
  cx <- ask
  liftIO $ atomically $ do
    Context{cxPubSub = pubsub,cxOutBox = chan} <- pure cx
    modifyTVar pubsub $ \m -> M.alter (\mbs -> let s = case mbs of {Just s -> s; _ -> []} in Just $ del [] chan s) topic m

pub topic a = do
  cx@Context{cxPubSub = pubsub} <- ask
  l <- liftIO $ atomically $ do
    m <- readTVar pubsub
    l <- pure $ case M.lookup topic m of {Just s -> s; _ -> []}
    forM_ l (\chan -> do {rChan <- dupTChan chan; writeTChan chan a})
    pure l
  return ()

fnd x [] = False
fnd x (y:ys) = if x == y then True else fnd x ys

ins x ys = if fnd x ys then ys else x:ys

del acc _ [] = acc
del acc x (y:ys) = if x == y then del acc x ys else del (y:acc) x ys

-- | Put data to the local state
put :: (B.Serialize bin) => BS.ByteString -> bin -> N2O f a ()
put k v = do
  cx <- ask
  liftIO $ atomically $ modifyTVar (cxDict cx) (\dict -> M.insert k (B.encode v) dict)

-- | Get data from the local state
get :: (B.Serialize bin) => BS.ByteString -> N2O f a (Maybe bin)
get k = do
  cx <- ReaderT return
  m <- liftIO $ atomically $ readTVar $ cxDict cx
  case m !? k of
    Just v -> case (B.decode v) of
                Right x -> return $ Just x
                _ -> return Nothing
    _ -> return Nothing

getContext :: N2O f a (Context f a)
getContext = do
  cx <- ask
  pure cx

-- | 'Context' constructor
mkCx = Context
  { cxReq = undefined
  , cxHandler = undefined
  , cxMiddleware = []
  , cxProtos = []
  , cxActions = undefined
  , cxDict = undefined
  , cxPubSub = undefined
  , cxOutBox = undefined
  , cxInBox = undefined
  , cxTid = undefined
  , cxSessions = undefined
  }

-- | 'Req' constructor
mkReq = Req { reqPath = "/", reqMeth = "GET", reqVers = "HTTP/1.1", reqHead = [] }

-- | NO-OP result
nop :: Result a
nop = Empty

-- | N2O protocol loop
protoRun :: f a -> [Proto f a] -> N2O f a (Result (f a))
protoRun = loop
  where
    loop :: f a -> [Proto f a] -> N2O f a (Result (f a))
    loop _ [] = return nop
    loop msg (proto:protos) = do
      res <- proto msg
      case res of
        Unknown -> loop msg protos
        Empty -> return Empty
        Reply msg1 -> return $ Reply msg1
        a -> loop msg protos

