module Network.N2O.Types where

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Binary as B
import Data.Map.Strict (Map, insert, (!?))

type N2OState = IORef (Map BS.ByteString BL.ByteString)
type N2O = N2OM N2OState IO

-- | Lightweight version of ReaderT from @transformers@ package
newtype N2OM state m a = N2OM { runN2O :: state -> m a }
instance Functor m => Functor (N2OM state m) where
  fmap f (N2OM g) = N2OM $ (\x -> fmap f (g x))
instance Applicative m => Applicative (N2OM state m) where
  pure = lift . pure
  (N2OM f) <*> (N2OM g) = N2OM $ \state -> f state <*> g state
instance Monad m => Monad (N2OM state m) where
  m >>= k = N2OM $ \state -> do
    a <- runN2O m state
    runN2O (k a) state

lift :: m a -> N2OM state m a
lift m = N2OM (const m)

put :: (B.Binary a) => BS.ByteString -> a -> N2O ()
put k v = do
  state <- N2OM return
  lift $ modifyIORef state (\m -> insert k (B.encode v) m)

get :: (B.Binary a) => BS.ByteString -> N2O (Maybe a)
get k = do
  state <- N2OM return
  m <- lift $ readIORef state
  case m !? k of
    Just v -> return $ Just (B.decode v)
    _ -> return Nothing

data Msg = MsgTxt TL.Text | MsgBin BL.ByteString | MsgInit BL.ByteString | MsgTerminate deriving (Show, Eq)

type Header = (BS.ByteString, BS.ByteString)
data Req = Req
  { reqPath :: BS.ByteString
  , reqMeth :: BS.ByteString
  , reqVers :: BS.ByteString
  , reqHead :: [Header]
  }

-- | Result of the message processing
data Rslt = Reply Msg | Ok | Unknown deriving (Show, Eq)

-- | N2O protocol
data Proto a b = Proto
  { protoInfo :: Msg -> Cx a b -> N2O (Rslt, Cx a b)
  , protoInit :: N2O ()
  }

data Cx a b = Cx
  { cxEvHnd :: a -> N2O b
  , cxProtos :: [Proto a b]
  , cxReq :: Req
  , cxHandlers :: [Cx a b -> Cx a b]
  , cxEncode :: b -> Msg
  , cxDecode :: Msg -> Maybe a
  }

