module Network.N2O.Protocols where

import Network.N2O.Internal
import qualified Data.ByteString.Lazy as L
import Data.BERT
import qualified Data.Binary as B

data Client a = Client a | Server a
--data Nitro linked = Pickle L.ByteString linked
data System = Init L.ByteString | Terminate

data N2O client = N2OClient (Client client) {-| N2ONitro (Nitro nitro)-} | N2OSystem System

instance (BERT client{-, BERT nitro-}) => BERT (N2O client{- nitro-}) where
    showBERT (N2OClient (Client c)) = TupleTerm [AtomTerm "client", showBERT c]
    showBERT (N2OClient (Server s)) = TupleTerm [AtomTerm "server", showBERT s]
    --  showBERT (N2ONitro (Nitro n)) = TupleTerm [AtomTerm "pickle", showBERT n]
    showBERT (N2OSystem (Init pid)) = TupleTerm [AtomTerm "init", BytelistTerm pid]
    showBERT (N2OSystem Terminate) = AtomTerm "terminate"
    readBERT (TupleTerm [AtomTerm "client", x]) = readClient x Client
    readBERT (TupleTerm [AtomTerm "server", x]) = readClient x Server
    readBERT (TupleTerm [AtomTerm "init", BytelistTerm pid]) = Right $ N2OSystem (Init pid)
    readBERT (AtomTerm "terminate") = Right $ N2OSystem Terminate
    readBERT _ = Left "N2O BERT instance: Unknown format"

readClient x f = case readBERT x of
                   Right term -> Right $ N2OClient (f term)
                   Left err -> Left err

-- | default decoder
defDecoder msg =
  case decodeBert msg of
    Just term -> case readBERT term of
                   Right x -> Just x
                   _ -> Nothing
    _ -> Nothing
-- | default encoder
defEncoder bs = MsgBin (B.encode (TupleTerm [AtomTerm "io", BytelistTerm bs, NilTerm]))

clientProto :: (Show client{-, Show nitro-}) => Proto (N2O client {-nitro-}) L.ByteString
clientProto = Proto { protoInit = return (), protoInfo = clientInfo }

systemProto :: Proto (N2O client) L.ByteString
systemProto = Proto { protoInit = return (), protoInfo = systemInfo }

clientInfo :: Msg -> Cx (N2O client{- nitro-}) L.ByteString -> IO (Rslt, Cx (N2O client{- nitro-}) L.ByteString)
clientInfo message cx@Cx{cxEvHnd=handle,cxEncode=encode,cxDecode=decode} = do
  case decode message of
    Just msg@(N2OClient cli) -> do
      reply <- handle msg
      return (Reply (encode reply), cx)
    _ -> return $ (Unknown, cx)

systemInfo :: Msg -> Cx (N2O client) L.ByteString -> IO (Rslt, Cx (N2O client) L.ByteString)
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
