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
  , cxActions :: BS.ByteString
  , cxDict :: M.Map BS.ByteString BS.ByteString
  } -> Context f a

-- | Result of the message processing
data Result a
  = Reply a
  | Ok
  | Unknown
  | Empty
  deriving (Show, Eq)

-- class Proto p where
--   protoInfo :: p -> (f :: * -> *) a -> N2O f a (Result (f a))

-- | N2O protocol handler
type Proto (f :: * -> *) a = (f a) -> N2O f a (Result (f a))

-- | Event data type
data Event a
  = Init
  | Message a
  | Terminate
  deriving Show

type N2O f a = ReaderT (IORef (Context f a)) IO

-- | Put data to the local state
put :: (B.Serialize bin) => BS.ByteString -> bin -> N2O f a ()
put k v = do
  ref <- ask
  liftIO $ modifyIORef ref (\st@Context{cxDict=dict} -> st{cxDict=(M.insert k (B.encode v) dict)})

-- | Get data from the local state
get :: (B.Serialize bin) => BS.ByteString -> N2O f a (Maybe bin)
get k = do
  ref <- ReaderT return
  st <- liftIO $ readIORef ref
  let m = cxDict st
  case m !? k of
    Just v -> case (B.decode v) of
                Right x -> return $ Just x
                _ -> return Nothing
    _ -> return Nothing

getContext :: N2O f a (Context f a)
getContext = do
  ref <- ask
  liftIO $ readIORef ref

-- | 'Context' constructor
mkCx = Context
  { cxReq = undefined
  , cxHandler = undefined
  , cxMiddleware = []
  , cxProtos = []
  , cxActions = ""
  , cxDict = M.empty
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

