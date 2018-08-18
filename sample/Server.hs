{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent         (forkIO, myThreadId)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad              (forM_)
import           Data.Binary                (Binary)
import           Data.Text                  (Text)
import           Fmt                        ((|+),(+|))
import           GHC.Generics               (Generic)
import           Network.N2O
import           Network.N2O.Nitro
import           Prelude                    (($), show, print, Maybe(Just))

data App = Chat Text | JustForFun deriving (Generic)
instance N2OMessage App
instance Binary App

main = runServer defaultConfig handle

handle Init _ = do
  insertBottom "app" panel{body=[textbox{id="msg"}
                                ,button{id="btn",body=[Text "Chat"],postback="Chat",source=["msg"]}
                                ,button{id="fun",body=[Text "Have fun"],postback="JustForFun"}
                                ]}

handle Message (Just (Chat x)) = do
  threadId <- liftIO $ myThreadId
  insertBottom "app" panel{body=[text ("" +| show threadId |+ ": " +| x |+ ""), br]}
  broadcast

handle Message (Just JustForFun) = alert "hahaha!"

handle _ _ = liftIO $ print "unknown message"
