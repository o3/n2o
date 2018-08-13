{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DefaultSignatures #-}

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
import           Prelude                    hiding (id, init)
import Data.Binary
import GHC.Generics

data App = Init | Destroy | Chat C8.ByteString deriving (Generic, Show)
instance N2OMessage App where
  init = Init
  destroy = Destroy
instance Binary App

main :: IO ()
main = do
  threadId <- myThreadId
  putStrLn $ "Main thread id: " +| show threadId |+ ""
  runServer defaultConfig handle

handle :: Handler App ()

handle Init = do
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
            , eventPostback = Nothing --getIndex $ Chat {}
            , eventSource = ["msg"]
            , eventType = "click"
            }
      ]
  liftIO $ print e
  send e

handle (Chat x) = do
  (LocalState {..}, _) <- ask
  -- Just (BytelistTerm x) <- q "msg"
  msg <-
    insertBottom
      "history"
      panel {body = [text $ clientId |+ ": " +| b2t x |+ "", br]}
  broadcast msg
  send "qi('msg').value='';"

handle Destroy = do
  (LocalState {..}, _) <- ask
  printIO $ "disconnected " ++ show clientId

-- handle msg = do
--   printIO "Protocol violation"
--   (alert $ "Unknown message: " +| show msg |+ "") >>= send

printIO x = liftIO $ putStrLn x

