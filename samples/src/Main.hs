{-# LANGUAGE TypeFamilies, OverloadedLists, OverloadedStrings, FlexibleContexts #-}
module Main (main) where

import Network.N2O
import Network.N2O.Util
import Network.N2O.WebSockets
import Network.N2O.Http
import Network.N2O.Protocols
import Network.N2O.Nitro
import Debug.Trace

data Example = Greet deriving (Show, Eq, Read)

main = runServer "localhost" 3000 cx

cx = createCx router

router cx@Cx{cxReq=Req{reqPath=path}} =
  let handler = case path of
                  "/ws/samples/static/index.html" -> index
                  "/ws/samples/static/about.html" -> about
                  _ -> index
  in traceShow path cx{cxHandler=handler}

index Init = do
  ev <- renderEvent Event{eventTarget="send",eventPostback=Greet,eventType="click",eventSource=["name"]}
  return $ "qi('system').innerText='What is your name?';" <> ev
index (Message Greet) = do
  Just name <- get "name" -- wf:q/1
  return $ "qi('system').innerText='Hello, " <> (jsEscape name) <> "!'"
about Init = do
  return $ "qi('app').innerText='This is the N2O Hello World App'"
