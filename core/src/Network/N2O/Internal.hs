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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Data.IORef
import Data.Map.Strict (Map, (!?), insert)
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Reader
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
data Context (f :: * -> *) a state = Context
  { cxHandler :: Event a -> N2O state (Result a)
  , cxReq :: Req
  , cxMiddleware :: [Context f a state -> Context f a state]
  , cxProtos :: [Proto (f a) state]
  , cxDePickle :: BL.ByteString -> Maybe a
  , cxPickle :: a -> BL.ByteString
  }

-- | Result of the message processing
data Result a
  = Reply a
  | Ok
  | Unknown
  | Empty
  deriving (Show, Eq)

-- | N2O protocol handler
type Proto a state = a -> N2O state (Result a)

-- | Event data type
data Event a
  = Init
  | Message a
  | Terminate
  deriving Show

type N2O state = ReaderT state IO

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
protoRun :: f a -> [Proto (f a) state] -> N2O state (Result (f a))
protoRun = loop []
  where
    loop _ _ [] = return nop
    loop acc msg (proto:protos) = do
      res <- proto msg
      case res of
        Unknown -> loop acc msg protos
        Empty -> return Empty
        Reply msg1 -> return $ Reply msg1
        a -> loop (a : acc) msg protos

