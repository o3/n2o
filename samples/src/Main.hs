{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import Network.N2O
import Network.N2O.WebSockets
import Network.N2O.Http
import Network.N2O.Protocols
import Network.N2O.Nitro
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Prelude hiding (id)
import Debug.Trace

data Example = Greet deriving (Show, Eq, Read, Generic, Binary)

main = runServer "localhost" 3000 cx

cx = createCx router

router cx@Cx{cxReq=Req{reqPath=path}} =
  let handler = case path of
                  "/ws/samples/static/index.html" -> index
                  "/ws/samples/static/about.html" -> about
                  _ -> index
  in traceShow path cx{cxHandler=handler}

index Init = do
  updateText "system" "What is your name?"
  wireEl button{id="send", postback=Just Greet, source=["name"]}
index (Message Greet) = do
  Just name <- get "name" -- wf:q/1
  updateText "system" ("Hello, " <> jsEscape name <> "!")
about Init =
  updateText "app" "This is the N2O Hello World App"
