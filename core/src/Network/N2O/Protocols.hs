module Network.N2O.Protocols where

import Network.N2O.Types
import Network.N2O.Internal
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as BS
import Data.BERT
import qualified Data.Binary as B
import qualified Data.Map.Strict as M

data Client a = Client a | Server a deriving (Show)
data Nitro = Init L.ByteString
 | Pickle { pickleSource  :: BS.ByteString
          , picklePickled :: L.ByteString
          , pickleLinked  :: (M.Map BS.ByteString L.ByteString)
          }
 deriving (Show)

data N2OProto a = N2OClient (Client a) | N2ONitro Nitro deriving (Show)

class Bert a where
  readBert :: Term -> Maybe a

instance (Bert a) => Bert (N2OProto a) where
    readBert (TupleTerm [AtomTerm "client", x]) = readClient x Client
    readBert (TupleTerm [AtomTerm "server", x]) = readClient x Server
    readBert (TupleTerm [AtomTerm "init", BytelistTerm pid]) = Just $ N2ONitro (Init pid)
--    readBert (AtomTerm "terminate") = Just $ Nitro Terminate
    readBert _ = Nothing

readClient x f = case readBert x of
                   Just term -> Just $ N2OClient (f term)
                   _ -> Nothing

-- | default decoder
defDecoder msg =
  case decodeBert msg of
    Just term -> case readBert term of
                   Just x -> Just x
                   _ -> Nothing
    _ -> Nothing
-- | default encoder
defEncoder bs = MsgBin (B.encode (TupleTerm [AtomTerm "io", BytelistTerm bs, NilTerm]))

clientProto :: (Show a) => Proto (N2OProto a) L.ByteString
clientProto = Proto { protoInit = return (), protoInfo = clientInfo }

nitroProto :: (Show a) => Proto (N2OProto a) L.ByteString
nitroProto = Proto { protoInit = return (), protoInfo = nitroInfo }

clientInfo :: (Show a) => Msg -> Cx (N2OProto a) L.ByteString -> N2O (Rslt, Cx (N2OProto a) L.ByteString)
clientInfo message cx@Cx{cxEvHnd=handle,cxEncode=encode,cxDecode=decode} = do
  let decoded = decode message
  case decoded of
    Just msg@(N2OClient cli) -> do
      lift $ print msg
      reply <- handle msg
      return (Reply (encode reply), cx)
    _ -> return $ (Unknown, cx)

nitroInfo :: (Show a) => Msg -> Cx (N2OProto a) L.ByteString -> N2O (Rslt, Cx (N2OProto a) L.ByteString)
nitroInfo message cx@Cx{cxEvHnd=handle,cxEncode=encode,cxDecode=decode} = do
  let decoded = decode message
  case decoded of
    Just msg@(N2ONitro (Init pid)) -> {- TODO: depickle -} do
      lift $ print msg
      reply <- handle msg
      return (Reply (encode reply), cx)
    Just msg@(N2ONitro (Pickle _source pickled linked)) -> do
      _ <- handle msg
      return (Ok, cx)
    _ -> return $ (Unknown, cx)

createCx router =
  mkCx{ cxHandlers = [router], cxProtos = [clientProto, nitroProto], cxEncode = defEncoder, cxDecode = defDecoder }
