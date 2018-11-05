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
module Network.N2O.Core (lift, ask, mkCx, mkReq, protoRun, {-put, get, getContext-}) where

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
  }

-- | 'Req' constructor
mkReq = Req { reqPath = "/", reqMeth = "GET", reqVers = "HTTP/1.1", reqHead = [] }

-- | NO-OP result
nop :: Result a
nop = Empty

-- | N2O protocol loop
protoRun :: (Monad m) => f a -> [Proto f a m] -> m (Result (f a))
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

-- | Execute a computation in a modified environment
local :: (state -> state) -> N2OT state m a -> N2OT state m a
local f m = N2OT $ runN2O m . f
