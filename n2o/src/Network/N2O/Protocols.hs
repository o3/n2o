module Network.N2O.Protocols where

import Network.N2O.Internal
import qualified Data.ByteString.Lazy as L
import Data.BERT
import qualified Data.Binary as B

data Client a = Client a | Server a
--data Nitro linked = Pickle L.ByteString linked
data System = Init L.ByteString | Terminate

data N2O a = N2OClient (Client a) {-| N2ONitro (Nitro nitro)-} | N2OSystem System

class Bert a where
  readBert :: Term -> Maybe a

instance (Bert a) => Bert (N2O a) where
    readBert (TupleTerm [AtomTerm "client", x]) = readClient x Client
    readBert (TupleTerm [AtomTerm "server", x]) = readClient x Server
    readBert (TupleTerm [AtomTerm "init", BytelistTerm pid]) = Just $ N2OSystem (Init pid)
    readBert (AtomTerm "terminate") = Just $ N2OSystem Terminate
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

clientProto :: (Show a) => Proto (N2O a) L.ByteString
clientProto = Proto { protoInit = return (), protoInfo = clientInfo }

systemProto :: Proto (N2O a) L.ByteString
systemProto = Proto { protoInit = return (), protoInfo = systemInfo }

clientInfo :: Msg -> Cx (N2O a) L.ByteString -> IO (Rslt, Cx (N2O a) L.ByteString)
clientInfo message cx@Cx{cxEvHnd=handle,cxEncode=encode,cxDecode=decode} = do
  case decode message of
    Just msg@(N2OClient cli) -> do
      reply <- handle msg
      return (Reply (encode reply), cx)
    _ -> return $ (Unknown, cx)

systemInfo :: Msg -> Cx (N2O a) L.ByteString -> IO (Rslt, Cx (N2O a) L.ByteString)
systemInfo message cx@Cx{cxEvHnd=handle,cxEncode=encode,cxDecode=decode} = do
  case decode message of
    Just msg@(N2OSystem (Init pid)) -> {- TODO: depickle -} do
      reply <- handle msg
      return (Reply (encode reply), cx)
    Just msg@(N2OSystem Terminate) -> do
      _ <- handle msg
      return (Ok, cx)
    _ -> return $ (Unknown, cx)

createCx router =
  mkCx{ cxHandlers = [router], cxProtos = [systemProto, clientProto], cxEncode = defEncoder, cxDecode = defDecoder }
