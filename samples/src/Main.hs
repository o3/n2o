{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import Network.N2O
import Network.N2O.Web
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data Example = Greet deriving (Show, Eq, Generic, Serialize)

main = runServer "localhost" 3000 cx

cx = mkCx{ cxMiddleware=[router]
         , cxProtos = [nitroProto]
         , cxDePickle = defDePickle
         , cxPickle = defPickle
         }

router cx@Context{cxReq=Req{reqPath=path}} =
  let handle = case path of
                  "/ws/samples/static/index.html" -> index
                  "/ws/samples/static/about.html" -> about
                  _ -> index
  in cx{cxHandler=handle}

index Init = do
  updateText "system" "What is your name?"
  wireEl button{id_="send", postback=Just Greet, source=["name"]}
index (Message Greet) = do
  Just name <- get "name" -- wf:q/1
  updateText "system" ("Hello, " <> jsEscape name <> "!")
index ev = liftIO $ putStrLn ("Unknown event" <> show ev) >> return Empty
about Init =
  updateText "app" "This is the N2O Hello World App"
about ev = do
  liftIO $ putStrLn ("Unknown event " <> show ev)
  return Empty
