{-# LANGUAGE KindSignatures #-}
module Network.N2O.Types where

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Binary as B
import Data.Map.Strict (Map, insert, (!?))

type Header = (BS.ByteString, BS.ByteString)

data Req = Req
  { reqPath :: BS.ByteString
  , reqMeth :: BS.ByteString
  , reqVers :: BS.ByteString
  , reqHead :: [Header]
  }

data Cx (f :: * -> *) (a :: *) (b :: *) = Cx
  { cxHandler :: W a -> N2O f a b b
  , cxReq :: Req
  , cxMiddleware :: [Cx f a b -> Cx f a b]
  , cxDePickle :: BL.ByteString -> Maybe a
  , cxPickle :: a -> BL.ByteString
  , cxProtos :: [Proto f a b]
  , cxEncoder :: Encoder b
  , cxDecoder :: Decoder (f a)
  , cxState :: Map BS.ByteString BL.ByteString
  }
type N2OState (f :: * -> *) (a :: *) (b :: *) = IORef (Cx f a b)
type N2O (f :: * -> *) (a :: *) (b :: *) = N2OM (N2OState f a b) IO

-- | Lightweight version of ReaderT from @transformers@ package
newtype N2OM state m a = N2OM { runN2O :: state -> m a }

instance Functor m => Functor (N2OM state m) where
  fmap f (N2OM g) = N2OM (fmap f . g)

instance Applicative m => Applicative (N2OM state m) where
  pure = lift . pure
  (N2OM f) <*> (N2OM g) = N2OM $ \state -> f state <*> g state

instance Monad m => Monad (N2OM state m) where
  m >>= k = N2OM $ \state -> do
    a <- runN2O m state
    runN2O (k a) state

data Msg = MsgTxt TL.Text | MsgBin BL.ByteString | MsgInit BL.ByteString | MsgTerminate deriving (Show, Eq)

-- | Result of the message processing
data Return = Reply Msg | Ok | Unknown deriving (Show, Eq)

-- | N2O protocol
newtype Proto (f :: * -> *) (a :: *) (b :: *) =
  Proto{ protoInfo :: f a -> N2O f a b Return }

data W a = Init | Message a | Terminate

type Encoder (a :: *) = a -> Msg
type Decoder (a :: *) = Msg -> Maybe a

lift :: m a -> N2OM state m a
lift m = N2OM (const m)

ask :: (Monad m) => N2OM state m state
ask = N2OM return

put :: (B.Binary bin) => BS.ByteString -> bin -> N2O f a b ()
put k v = do
  state <- ask
  lift $ modifyIORef state (\cx@Cx{cxState=m} -> cx{cxState=insert k (B.encode v) m})

get :: (B.Binary bin) => BS.ByteString -> N2O f a b (Maybe bin)
get k = do
  state <- N2OM return
  cx <- lift $ readIORef state
  let mp = cxState cx
  case mp !? k of
    Just v -> return $ Just (B.decode v)
    _ -> return Nothing
