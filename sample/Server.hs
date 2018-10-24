module Main (main) where

import Network.N2O
import Network.N2O.Util
import Data.BERT

main = runServer "localhost" 3000 mkCx { cxHandlers = [router], cxProtos = [proto1] }

router :: Cx -> Cx
router cx = cx{ cxEvHnd = event } -- we have single (index) page only

-- | Here's our event handler
event :: Term -> IO Term

event ["init", _] = do
  return $ BytelistTerm "qi('system').innerText='What is your name?'"

event ["client", ["greet", BytelistTerm name]] = do
    return $ BytelistTerm ("qi('system').innerText='Hello, " <> (jsEscape name) <> "!'")

event ev = do
  print ev -- print event and reply with empty string
  return []

-- | -----------------------------

proto1 :: Proto
proto1 = Proto
  { protoInit = return ()
  , protoInfo = \term cx -> do
      rep <- (cxEvHnd cx) term
      return ("reply", ["io", rep, []], cx) -- reply with IO message
  }
