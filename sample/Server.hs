module Main (main) where

import Network.N2O
import Network.N2O.Util
import Data.BERT

conf = mkHttpConf { httpPort = 3000, httpHost = "localhost" }
main = runServer conf defaultCx { cxHandlers = [router], cxProtos = [proto1] }

router :: Cx -> Cx
router cx = cx{ cxEvHnd = EvHnd { event = handle } } -- we have single (index) page only

-- | Here's our event handler

handle ["init", _] = do
  return $ BytelistTerm "qi('system').innerText='What is your name?'"

handle ["client", ["greet", BytelistTerm name]] = do
    return $ BytelistTerm ("qi('system').innerText='Hello, " <> (jsEscape name) <> "!'")

handle ev = do
  print ev -- print event and reply with empty string
  return []

-- | -----------------------------

proto1 :: Proto
proto1 = Proto
  { protoInit = return ()
  , protoInfo = \term cx@Cx{cxEvHnd=cxEvHnd} -> do
      rep <- (event cxEvHnd) term
      return ("reply", ["io", rep, []], cx) -- reply with IO message
  }
