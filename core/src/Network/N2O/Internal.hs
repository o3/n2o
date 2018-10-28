{-# LANGUAGE OverloadedStrings #-}
module Network.N2O.Internal where

import Data.BERT
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import Control.Exception (SomeException)
import Network.N2O.Types

-- | @Term -> Msg@ encoder
encodeBert :: Term -> Msg
encodeBert = MsgBin . B.encode
-- | @Term -> Msg@ decoder
decodeBert :: Msg -> Maybe Term
decodeBert (MsgBin bin) =
  case B.decodeOrFail bin of
    Right (_,_,term) -> Just term
    _ -> Nothing
decodeBert (MsgInit pid) = Just $ TupleTerm [AtomTerm "init", BytelistTerm pid]
decodeBert MsgTerminate = Just $ AtomTerm "terminate"
decodeBert _ = Nothing

mkCx = Cx
  { cxReq = undefined
  , cxEvHnd = undefined
  , cxHandlers = []
  , cxProtos = []
  , cxEncode = undefined
  , cxDecode = undefined
  }

mkReq = Req { reqPath = "/", reqMeth = "GET", reqVers = "HTTP/1.1", reqHead = [] }

nop :: Cx a b -> (Rslt, Cx a b)
nop cx = (Reply (MsgBin BSL.empty), cx)

protoRun :: Msg -> Cx a b -> N2O (Rslt, Cx a b)
protoRun msg cx = go [] msg cx (cxProtos cx)
  where
    go :: [(Rslt, Cx a b)] -> Msg -> Cx a b -> [Proto a b] -> N2O (Rslt, Cx a b)
    go _ _ state [] = return $ nop state
    go acc msg state (proto:protos) = do
        res <- protoInfo proto msg state
        case res of
            (Unknown , _) -> go acc msg state protos
            (Reply msg1, state1) -> return $ (Reply msg1, state1)
            a -> go (a:acc) msg state protos
