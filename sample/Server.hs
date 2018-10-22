{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main (main) where

import Network.N2O
import Data.BERT
import qualified Network.WebSockets as WS
import Prelude hiding (init)
import Data.ByteString.Char8 (unpack)

conf = mkHttpConf {httpPort = 3000, httpHost = "localhost", httpHandler = handler}
main = runServer conf

handler req respond
  | needUpgrade req = do
      -- make pending ws request
      pending <- mkPending WS.defaultConnectionOptions req
      let cx = defaultCx {cxReq = req, cxEvHnd = index, cxHandlers = [router], cxProtos = [proto1]}
      -- n2o stream app
      wsApp cx pending
  | True = fileResp (preparePath $ unpack $ reqPath req) respond
  where
    preparePath ('/':path) = path
    preparePath path = path

router :: Cx -> Cx
router cx@Cx{..} = cx{cxEvHnd = index}

index :: EvHnd
index = EvHnd
  { event = \ev -> do {print ev; return NilTerm} -- print an event and reply with empty response
  }

proto1 :: Proto
proto1 = Proto
  { protoInit = return ()
  -- pass message to the event handler as is and reply with result
  , protoInfo = \term cx@Cx{..} -> do {rep <- (event cxEvHnd) term; return (reply, rep, cx)}
  }
