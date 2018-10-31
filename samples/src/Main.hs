{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}
module Main (main) where

import Network.N2O
import Network.N2O.Web
import Network.N2O.Protocols hiding (Init)
import Network.N2O.Nitro
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Prelude hiding (id)

data Example = Greet deriving (Show, Eq, Read, Generic, Binary)

main = runServer "localhost" 3000 cx

cx = createCx router

router cx@Context{cxReq=Req{reqPath=path}} =
  let handle = case path of
                  "/ws/samples/static/index.html" -> index
                  "/ws/samples/static/about.html" -> about
                  _ -> index
  in cx{cxHandler=handle}

index Init = do
  updateText "system" "What is your name?"
  wireEl button{id="send", postback=Just Greet, source=["name"]}
index (Message Greet) = do
  Just name <- get "name" -- wf:q/1
  updateText "system" ("Hello, " <> jsEscape name <> "!")
index _ = lift $ putStrLn "Unknown event" >> return Empty
about Init =
  updateText "app" "This is the N2O Hello World App"
about _ = lift $ putStrLn "Unknown event" >> return Empty
