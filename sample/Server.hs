{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent         (forkIO, myThreadId)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask)
import           Control.Monad              (forM_)
import           Data.Binary                (Binary)
import qualified Data.Text                  as T
import           Fmt                        ((|+),(+|))
import           GHC.Generics               (Generic)
import           Network.N2O
import           Network.N2O.Nitro
import           Prelude                    (($), show, print)

data App = Init | Destroy | Chat T.Text | JustForFun deriving (Generic)
instance N2OMessage App where
  init = Init
  destroy = Destroy
instance Binary App

main = runServer defaultConfig handle

handle Init = do
  insertBottom "app" panel{body=[textbox{id="msg"}
                                ,button{id="btn",body=[Text "Chat"],postback="Chat",source=["msg"]}
                                ,button{id="fun",body=[Text "Have fun"],postback="JustForFun"}
                                ]}

handle (Chat x) = do
  threadId <- liftIO $ myThreadId
  insertBottom "app" panel{body=[text ("" +| show threadId |+ ": " +| x |+ ""), br]}
  broadcast

handle JustForFun = alert "\\'hahaha!"

handle _ = liftIO $ print "unknown message"

