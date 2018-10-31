{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : Network.N2O.Core
Description : Core functions
Copyright   : (c) Marat Khafizov, 2018
License     : BSD 3-Clause
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

Core functions

-}
module Network.N2O.Core (lift, ask, put, get, mkCx, mkReq, protoRun) where

import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import Control.Exception (SomeException)
import Network.N2O.Types
import Data.Map.Strict (insert, (!?))

-- | 'Context' constructor
mkCx = Context
  { cxReq = undefined
  , cxHandler = undefined
  , cxMiddleware = []
  , cxDePickle = undefined
  , cxPickle = undefined
  , cxProtos = []
  , cxState = M.empty
  }

-- | 'Req' constructor
mkReq = Req { reqPath = "/", reqMeth = "GET", reqVers = "HTTP/1.1", reqHead = [] }

-- | NO-OP result
nop :: Result a
nop = Empty

-- | N2O protocol loop
protoRun :: f a -> [Proto f a] -> N2O f a (Result (f a))
protoRun = loop []
  where
    loop _ _ [] = return nop
    loop acc msg (proto:protos) = do
      res <- protoInfo proto msg
      case res of
        Unknown -> loop acc msg protos
        Empty -> return Empty
        Reply msg1 -> return $ Reply msg1
        a -> loop (a : acc) msg protos

-- | Lift underlying monad to the N2O monad
lift :: m a -> N2OT state m a
lift m = N2OT (const m)

-- | Get current state (env)
ask :: (Monad m) => N2OT state m state
ask = N2OT return

getContext = do
  ref <- ask
  lift $ readIORef ref

-- | Put data to the local state
put :: (B.Binary bin) => BS.ByteString -> bin -> N2O f a ()
put k v = do
  state <- ask
  lift $ modifyIORef state (\cx@Context{cxState=m} -> cx{cxState=insert k (B.encode v) m})

-- | Get data from the local state
get :: (B.Binary bin) => BS.ByteString -> N2O f a (Maybe bin)
get k = do
  state <- N2OT return
  cx <- lift $ readIORef state
  let mp = cxState cx
  case mp !? k of
    Just v -> return $ Just (B.decode v)
    _ -> return Nothing
