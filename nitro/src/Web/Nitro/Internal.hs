{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances
           , FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
module Web.Nitro.Internal where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Serialize as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Web.Nitro.Types (Element(MkTr), Event)
import Web.Nitro.Render

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

update :: (NITRO m, B.Serialize a) => BS.ByteString -> [Element a] -> m ()
update target elems = do
  (content,js) <- render_ elems
  addAction $ "(function(){qi('" <> target <> "').outerHTML='" <> content <> "';}();" <> js

-- | Update text content of the element with the specified @id@
updateText :: (NITRO m) => BS.ByteString -> T.Text -> m ()
updateText target s = do
  addAction $
    "qi('" <> target <> "').innerText='"
    <> T.encodeUtf8 s <> "';"

insertTagTop :: (NITRO m, B.Serialize a) => BS.ByteString -> BS.ByteString -> [Element a] -> m ()
insertTagTop tag target elems = do
  (content,js) <- render_ elems
  addAction $
    "qi('" <> target <> "').insertBefore(" <>
    "(function(){ var div = qn('" <> tag <> "'); div.innerHTML = '" <> content <> "'; return div.firstChild; })()," <>
    "qi('" <>  target <> "').firstChild);" <> js

insertTagBottom :: (NITRO m, B.Serialize a) => BS.ByteString -> BS.ByteString -> [Element a] -> m ()
insertTagBottom tag target elems = do
  (content,js) <- render_ elems
  addAction $
    "(function(){ var div = qn('" <> tag <> "'); div.innerHTML = '" <> content <>
    "';qi('" <>  target <> "').appendChild(div.firstChild); })();" <> js

insertX f target elems@(MkTr{}:_) = f "tbody" target elems
insertX f target elems = f "div" target elems

insertTop :: (NITRO m, B.Serialize a) => BS.ByteString -> [Element a] -> m ()
insertTop = insertX insertTagTop

insertBottom :: (NITRO m, B.Serialize a) => BS.ByteString -> [Element a] -> m ()
insertBottom = insertX insertTagBottom

insertAdjacent :: (NITRO m, B.Serialize a) => BS.ByteString -> BS.ByteString -> [Element a] -> m ()
insertAdjacent position target elems = do
  (content,js) <- render_ elems
  addAction $
    "qi('" <> target <> "').insertAdjacentHTML('" <> position <> "', '" <> content <> "');" <> js

insertBefore :: (NITRO m, B.Serialize a) => BS.ByteString -> [Element a] -> m ()
insertBefore = insertAdjacent "beforebegin"

insertAfter :: (NITRO m, B.Serialize a) => BS.ByteString -> [Element a] -> m ()
insertAfter = insertAdjacent "afterend"

insertTop1 target elem = insertTop target [elem]
insertBottom1 target elem = insertBottom target [elem]
insertBefore1 target elem = insertBefore target [elem]
insertAfter1 target elem = insertAfter target [elem]

clear :: (NITRO m) => BS.ByteString -> m ()
clear target = addAction $ "(function(){var x = qi('" <> target <> "'); while (x.firstChild) x.removeChild(x.firstChild);})();"

remove :: (NITRO m) => BS.ByteString -> m ()
remove target = addAction $ "(function(){var x=qi('" <> target <> "'); x && x.parentNode.removeChild(x);})();"

redirect url = addAction $ "(function(){document.location = '" <> url <> "';})();"
display elem status = addAction $
  "(function(){var x = qi('" <> elem <> "'); if (x) x.style.display = '" <> status <> "';})();"
show_ elem = display elem "block"
hide elem = display elem "none"
