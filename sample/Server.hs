module Main (main) where

import Network.N2O
import Network.N2O.Util
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
      let cx = defaultCx {cxReq = req, cxHandlers = [router], cxProtos = [proto1]}
      -- n2o stream app
      wsApp cx pending
  | True = fileResp (preparePath $ unpack $ reqPath req) respond
  where
    preparePath ('/':path) = path
    preparePath path = path

router :: Cx -> Cx
router cx = cx{cxEvHnd = index} -- we have single (index) page only

index :: EvHnd
index = EvHnd { event = handle }

-- | Here's our event handler

handle ["init", _] = do
  return $ BytelistTerm "qi('system').innerHTML='What is your name?'"

handle ["client", ["greet", BytelistTerm name]] = do
    return $ BytelistTerm ("qi('system').innerHTML='Hello, " <> (jsEscape name) <> "!'")

handle ev = do
  print (["client", ["greet", BytelistTerm "foo"]]::Term)
  print ev -- print event and reply with empty string
  return []

-- | -----------------------------

proto1 :: Proto
proto1 = Proto
  { protoInit = return ()
  , protoInfo = \term cx@Cx{cxEvHnd=cxEvHnd} -> do
      rep <- (event cxEvHnd) term
      return (reply, ["io", rep, []], cx) -- reply with IO message
  }
