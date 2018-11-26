{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances
           , FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
module Web.Nitro.Internal where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Serialize as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Web.Nitro.Elements
import Web.Nitro.Elements.Render

class (MonadIO m) => NITRO m where
  getActions :: m BS.ByteString
  putActions :: BS.ByteString -> m ()

addAction a = do
  acts <- getActions
  putActions $ a <> acts

-- | Wire action
wire :: (Renderable r, (NITRO m)) => r -> m ()
wire r = do
  (_,actions) <- render_ r
  addAction actions

-- | Update text content of the element with the specified @id@
updateText :: (NITRO m) => BS.ByteString -> T.Text -> m ()
updateText target s = do
  addAction $
    "qi('" <> target <> "').innerText='"
    <> T.encodeUtf8 s <> "';"

insertBottom :: (NITRO m, B.Serialize a) => BS.ByteString -> Element a -> m ()
insertBottom target elem = do
  (content,js) <- render_ elem
  addAction $
    "(function(){ var div = qn('div'); div.innerHTML = '" <>
    content <> "';qi('" <>  target <>
    "').appendChild(div.firstChild); })();" <> js
