{-# LANGUAGE KindSignatures #-}

{-|
Module      : Network.N2O.Types
Description : Basic types
Copyright   : (c) Marat Khafizov, 2018
License     : BSD-3
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

Basic types

-}
module Network.N2O.Types where

import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Map.Strict (Map, (!?), insert)
import qualified Data.Text.Lazy as TL

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
data Context (f :: * -> *) a = Context
  { cxHandler :: Event a -> N2O f a (Result a)
  , cxReq :: Req
  , cxMiddleware :: [Context f a -> Context f a]
  , cxProtos :: [Proto f a]
  , cxDePickle :: BL.ByteString -> Maybe a
  , cxPickle :: a -> BL.ByteString
  , cxState :: Map BS.ByteString BL.ByteString
  }

-- | Result of the message processing
data Result a
  = Reply a
  | Ok
  | Unknown
  | Empty
  deriving (Show, Eq)

-- | N2O protocol handler
newtype Proto f a = Proto
  { protoInfo :: f a -> N2O f a (Result (f a))
  }

-- | Event data type
data Event a
  = Init
  | Message a
  | Terminate

-- | Local mutable state
type State f a = IORef (Context f a)

-- | 'N2OT' over 'IO' with 'N2OState' as env
type N2O f a = N2OT (State f a) IO

-- | Reader monad transformer
newtype N2OT state m a = N2OT
  { runN2O :: state -> m a
  }

instance Functor m => Functor (N2OT state m) where
  fmap f (N2OT g) = N2OT (fmap f . g)

instance Applicative m => Applicative (N2OT state m) where
  pure = N2OT . const . pure
  (N2OT f) <*> (N2OT g) = N2OT $ \state -> f state <*> g state

instance Monad m => Monad (N2OT state m) where
  m >>= k =
    N2OT $ \state -> do
      a <- runN2O m state
      runN2O (k a) state
