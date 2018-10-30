{-# LANGUAGE KindSignatures #-}
module Network.N2O.Types where

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Binary as B
import Data.Map.Strict (Map, insert, (!?))

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
data Context (f :: * -> *) (a :: *) = Context
  { cxHandler :: Event a -> N2O f a BL.ByteString
  , cxReq :: Req
  , cxMiddleware :: [Context f a -> Context f a]
  , cxDePickle :: BL.ByteString -> Maybe a
  , cxPickle :: a -> BL.ByteString
  , cxProtos :: [Proto f a]
  , cxEncoder :: Encoder BL.ByteString
  , cxDecoder :: Decoder (f a)
  , cxState :: Map BS.ByteString BL.ByteString
  }

-- | Local mutable state
type State f a = IORef (Context f a)

-- | 'N2OM' over 'IO' with 'N2OState' as env
type N2O f a = N2OM (State f a) IO

-- | Lightweight version of @ReaderT@ from @transformers@ package
newtype N2OM state m a = N2OM { runN2O :: state -> m a }

instance Functor m => Functor (N2OM state m) where
  fmap f (N2OM g) = N2OM (fmap f . g)

instance Applicative m => Applicative (N2OM state m) where
  pure = N2OM . const . pure
  (N2OM f) <*> (N2OM g) = N2OM $ \state -> f state <*> g state

instance Monad m => Monad (N2OM state m) where
  m >>= k = N2OM $ \state -> do
    a <- runN2O m state
    runN2O (k a) state

-- | Message datatype, includes text messages, binary messages and some control messages
data Message = TextMessage TL.Text       | BinaryMessage BL.ByteString
             | InitMessage BL.ByteString | TerminateMessage
             deriving (Show, Eq)

-- | Result of the message processing
data Result = Reply Message | Ok | Unknown deriving (Show, Eq)

-- | N2O protocol handler
newtype Proto f a = Proto { protoInfo :: f a -> N2O f a Result }

-- | Event data type
data Event a = Init | Message a | Terminate

-- | Message encoder. Encode data structure as raw message
type Encoder a = a -> Message

-- | Interpret raw message as a data structure
type Decoder a = Message -> Maybe a
