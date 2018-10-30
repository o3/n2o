{-# LANGUAGE ScopedTypeVariables #-}
module Network.N2O.Protocols where

import Network.N2O.Types
import Network.N2O.Internal
import Network.N2O.Nitro
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.BERT
import Data.IORef
import qualified Data.Binary as B
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Base64.Lazy as B64
import Control.Monad (forM_)

data Nitro a =
   I L.ByteString
 | P { pickleSource  :: L.ByteString
     , picklePickled :: L.ByteString
     , pickleLinked :: M.Map BS.ByteString L.ByteString
     }
 deriving (Show)

newtype N2OProto a = N2ONitro (Nitro a) deriving (Show)

readBert (TupleTerm [AtomTerm "init", BytelistTerm pid]) = Just $ N2ONitro (I pid)
readBert (TupleTerm [AtomTerm "pickle", BinaryTerm source, BinaryTerm pickled, ListTerm linked]) =
    Just $ N2ONitro (P source pickled (convert linked))
  where
    convert [] = M.empty
    convert (TupleTerm [AtomTerm k, BytelistTerm v] : vs) = M.insert (C8.pack k) v (convert vs)
readBert _ = Nothing

nitroProto :: (Show a, B.Binary a) => Proto N2OProto a L.ByteString
nitroProto = Proto { protoInfo = nitroInfo }

nitroInfo :: forall a. (Show a, B.Binary a) => N2OProto a -> N2O N2OProto a L.ByteString Return
nitroInfo message = do
  ref <- ask
  cx@Cx {cxHandler = handle, cxEncoder = encode, cxDePickle = dePickle} <- lift $ readIORef ref
  lift $ putStrLn ("NITRO : " ++ show message)
  case message of
    msg@(N2ONitro (I pid)) -> do
      handle Init
      actions <- getActions
      Reply . encode <$> renderActions' actions
    msg@(N2ONitro (P _source pickled linked)) -> do
      forM_ (M.toList linked) (uncurry put)
      case dePickle pickled of
        Just x -> do
          handle (Message x)
          actions <- getActions
          Reply . encode <$> renderActions' actions
        _ -> return Unknown
  where
    renderActions' actions =
      case actions of
        [] -> return L.empty
        actions -> do
          putActions []
          first <- renderActions actions
          actions2 <- getActions
          second <- renderActions actions2
          putActions []
          return $ first <> CL8.pack ";" <> second

createCx router = mkCx
  { cxMiddleware = [router]
  , cxProtos = [nitroProto]
  , cxDecoder = defDecoder
  , cxEncoder = defEncoder
  , cxDePickle = defDePickle
  , cxPickle = defPickle
  }

-- | default decoder
defDecoder msg =
  case decodeBert msg of
    Just term -> case readBert term of
                   Just x -> Just x
                   _ -> Nothing
    _ -> Nothing
-- | default encoder
defEncoder bs = MsgBin (B.encode (TupleTerm [AtomTerm "io", BytelistTerm bs, NilTerm]))

defPickle :: (Show a) => a -> L.ByteString
defPickle = B64.encode . CL8.pack . show

defDePickle :: (Read a) => L.ByteString -> Maybe a
defDePickle bs =
  case B64.decode bs of
    Right x -> Just $ read $ CL8.unpack x
    _ -> Nothing

getActions :: (B.Binary a) => N2O f a b [Action a]
getActions = do
  mbActions <- get (C8.pack "actions")
  return $
    case mbActions of
           Just actions -> actions
           _ -> []

putActions :: (B.Binary a) => [Action a] -> N2O f a b ()
putActions = put (C8.pack "actions")
