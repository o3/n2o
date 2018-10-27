{-# LANGUAGE TypeFamilies, OverloadedLists, OverloadedStrings #-}
module Main (main) where

import Network.N2O
import Network.N2O.Util
import Network.N2O.WebSockets
import Network.N2O.Http
import Network.N2O.Protocols
import Data.BERT
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as L

main = runServer "localhost" 3000 (createCx router)

data Example = Greet L.ByteString deriving (Show, Eq)
instance BERT Example where
  showBERT = undefined -- we don't need this
  readBERT (TupleTerm [AtomTerm "greet", BytelistTerm name]) = Right $ Greet name
  readBERT _ = Left "Unknown term"

router :: Cx (N2O Example) L.ByteString -> Cx (N2O Example) L.ByteString
router cx = cx{ cxEvHnd = event } -- we have single (index) page only

event (N2OSystem system) = handleSystem system -- ^ handle system messages
event (N2OClient client) = handleClient client -- ^ handle client messages
handleSystem (Init _) =
  return "qi('system').innerText='What is your name?'"
handleSystem Terminate = return ""
handleClient(Client (Greet name)) =
  return $ "qi('system').innerText='Hello, " <> (jsEscape name) <> "!'"
