{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Network.N2O.Internal where

import Data.BERT
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import Network.Socket (Socket)

data Msg = MsgTxt TL.Text | MsgBin BSL.ByteString | MsgInit BSL.ByteString | MsgTerminate deriving (Show, Eq)
data Codec a b = Codec { dec :: Msg -> Maybe a, enc :: b -> Msg }

-- | @Msg -> Term -> Msg@ encoder/decoder
bertCodec :: Codec Term Term
bertCodec = Codec { dec = decode', enc = MsgBin . B.encode }
  where
    decode' (MsgBin bin) =
      case B.decodeOrFail bin of
        Right (_,_,term) -> Just term
        _ -> Nothing
    decode' (MsgInit pid) = Just $ TupleTerm [AtomTerm "init", BytelistTerm pid]
    decode' MsgTerminate = Just $ AtomTerm "terminate"
    decode' _ = Nothing

data Cx a b = Cx
  { cxEvHnd :: a -> IO b
  , cxProtos :: [Proto a b]
  , cxReq :: Req
  , cxHandlers :: [Cx a b -> Cx a b]
  , cxCodec :: Codec a b
  }
mkCx = Cx { cxReq = undefined, cxEvHnd = undefined, cxHandlers = [], cxProtos = [], cxCodec = undefined }
type Header = (BS.ByteString, BS.ByteString)
data Req = Req
  { reqPath :: BS.ByteString
  , reqMeth :: BS.ByteString
  , reqVers :: BS.ByteString
  , reqHead :: [Header]
  , reqSock :: Socket
  }

mkReq = Req { reqPath = "/", reqMeth = "GET", reqVers = "HTTP/1.1", reqHead = [], reqSock = undefined }

data Resp = Resp
  { respCode :: Int
  , respHead :: [Header]
  , respBody :: BS.ByteString
  } deriving (Show)

mkResp = Resp { respCode = 200, respHead = [], respBody = BS.empty }

-- | Result of the message processing
data Rslt = Reply Msg | Ok | Unknown deriving (Show, Eq)

-- | N2O protocol
data Proto a b = Proto
  { protoInfo :: Msg -> Cx a b -> IO (Rslt, Cx a b)
  , protoInit :: IO ()
  }

protoRun :: Msg -> Cx a b -> IO (Rslt, Cx a b)
protoRun msg cx = go [] msg cx (cxProtos cx)
  where
    nop :: Cx a b -> (Rslt, Cx a b)
    nop cx = (Reply (MsgBin BSL.empty), cx)
    go :: [(Rslt, Cx a b)] -> Msg -> Cx a b -> [Proto a b] -> IO (Rslt, Cx a b)
    go _ _ state [] = return $ nop state
    go acc msg state (proto:protos) = do
        res <- protoInfo proto msg state
        case res of
            (Unknown , _) -> go acc msg state protos
            (Reply msg1, state1) -> return $ (Reply msg1, state1)
            a -> go (a:acc) msg state protos
