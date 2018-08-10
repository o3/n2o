{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Concurrent         (forkIO, myThreadId)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask)
import           Data.BERT
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text                  as T
import           Fmt
import qualified Network.HTTP.Types         as Http
import           Network.N2O
import           Network.N2O.Nitro
import qualified Network.Wai                as Wai
import           Prelude                    hiding (id)

main :: IO ()
main = do
  threadId <- myThreadId
  putStrLn $ "Main thread id: " +| show threadId |+ ""
  runServer defaultConfig handle

handle :: Handler ()

handle ["init"] = do
  threadId <- liftIO myThreadId
  (LocalState {..}, _) <- ask
  printIO $ "connected " +| show clientId |+ ", " +| show threadId |+ ""
  e <-
    mconcat
      [ insertBottom
          "app"
          panel
            { body =
                [ panel {id = "history"}
                , textbox {id = "msg"}
                , button {id = "btn", body = [Text "Chat"]}
                ]
            }
      , renderEvent
          event
            { eventTarget = "btn"
            , eventPostback = "chat"
            , eventSource = ["msg"]
            , eventType = "click"
            }
      ]
  liftIO $ print e
  send e

handle ["chat"] = do
  (LocalState {..}, _) <- ask
  Just (BytelistTerm x) <- q "msg"
  msg <-
    insertBottom
      "history"
      panel {body = [text $ clientId |+ ": " +| b2t x |+ "", br]}
  broadcast msg
  send "qi('msg').value='';"

handle ["disconnect"] = do
  (LocalState {..}, _) <- ask
  printIO $ "disconnected " ++ show clientId

handle msg = do
  printIO "Protocol violation"
  (alert $ "Unknown message: " +| show msg |+ "") >>= send

printIO x = liftIO $ putStrLn x
