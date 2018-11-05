{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}

{-|
Module      : Network.N2O.Nitro
Description : Nitro DSL
Copyright   : (c) Marat Khafizov, 2018
License     : BSD-3
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

Nitro DSL to build interactive user interfaces

-}
module Network.N2O.Nitro where

import Control.Monad (forM_, void)
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.IORef
import Data.List (intercalate)
import Data.Map.Strict ((!?))
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.N2O hiding (Event)
import Network.N2O.Nitro.Elements (Element(..),render)
import Network.N2O.Nitro.Internal
import Prelude hiding (id)

instance (B.Binary a) => B.Binary (Element a)

-- | Action that can be rendered as JavaScript events
data Action a
  = AEvent (Event a)
  | AElement (Element a)
  | ARaw BL.ByteString
  deriving (Show, Generic)

instance (B.Binary a) => B.Binary (Action a)

-- | A JavaScript event
data Event a = Event
  { eventTarget   :: BS.ByteString
  , eventPostback :: a
  , eventType     :: BS.ByteString
  , eventSource   :: [BS.ByteString]
  } deriving (Show, Generic)

instance (B.Binary a) => B.Binary (Event a)

-- | Wire an element
wireEl :: (B.Binary a) => Element a -> N2O f a (Result a)
wireEl = wire . AElement

-- | Wire action
wire :: forall f a. (B.Binary a) => Action a -> N2O f a (Result a)
wire a = do
  actions <- getActions
  putActions (a : actions)
  return Empty

-- | Render list of actions to JavaScript
renderActions :: (B.Binary a) => [Action a] -> N2O f a BL.ByteString
renderActions [] = return ""
renderActions (a:as) = do
  r <- renderAction a
  rs <- renderActions as
  return (r <> ";" <> rs)

-- | Render an action
renderAction :: (B.Binary a) => Action a -> N2O f a BL.ByteString
renderAction (ARaw bs) = return bs
renderAction (AEvent ev) = renderEvent ev
renderAction (AElement el) = do
  case postback el of
    Nothing -> return ()
    Just pb -> void (wire $ AEvent Event
        {eventType = "click", eventPostback = pb, eventTarget = id el, eventSource = source el})
  return ""

-- | Render list of elements to the HTML
renderElements :: (B.Binary a) => [Element a] -> N2O f a BL.ByteString
renderElements [] = return ""
renderElements (e:es) = do
  r <- renderElement e
  rs <- renderElements es
  return (r <> rs)

-- | Render element to the HTML
renderElement :: (B.Binary a) => Element a -> N2O f a BL.ByteString
renderElement el = return $ render el

-- | Render event
renderEvent :: Event a -> N2O f a BL.ByteString
renderEvent Event {..} = do
  ref <- ask
  cx@Context {cxPickle = pickle} <- lift $ readIORef ref
  case eventSource of
    [] -> return BL.empty
    src ->
      return $
      "{ var x=qi('" <> BL.fromStrict eventTarget <> "'); x && x.addEventListener('" <> BL.fromStrict eventType <>
      "',function(event){ if (validateSources([" <> strJoin (map (\x -> "'" <> x <> "'") src) <>
      "])) { ws.send(enc(tuple(atom('pickle'),bin('" <> BL.fromStrict eventTarget <>
      "'),bin('" <> pickle eventPostback <>
      "'),[" <> strJoin (map renderSource src) <>
      "]))); } else console.log('Validation error'); })}"
  where
    renderSource :: BS.ByteString -> BS.ByteString
    renderSource s = "tuple(atom('" <> s <> "'),querySource('" <> s <> "'))"
    strJoin :: [BS.ByteString] -> BL.ByteString
    strJoin = BL.fromStrict . BS.intercalate ","

-- | Update text content of the element with the specified @id@
updateText :: (B.Binary a) => BS.ByteString -> TL.Text -> N2O f a (Result a)
updateText target s = wire
  (ARaw ("qi('" <> BL.fromStrict target <> "').innerText='"
         <> TL.encodeUtf8 s <> "'"))

insertBottom :: (B.Binary a) => BS.ByteString -> Element a -> N2O f a (Result a)
insertBottom target elem = do
  content <- renderElement elem
  let action =
        "(function(){ var div = qn('div'); div.innerHTML = '" <>
        TL.decodeUtf8 content <> "';qi('" <>
        TL.decodeUtf8 (BL.fromStrict target) <>
        "').appendChild(div.firstChild); })();"
  wire $ ARaw $ TL.encodeUtf8 action

-- | Default pickler
defPickle :: (Show a) => a -> BL.ByteString
defPickle = B64.encode . CL8.pack . show

-- | Default depickler
defDePickle :: (Read a) => BL.ByteString -> Maybe a
defDePickle bs =
  case B64.decode bs of
    Right x -> Just $ read $ CL8.unpack x
    _ -> Nothing

-- | Get action list from the local mutable state
getActions :: (B.Binary a) => N2O f a [Action a]
getActions = do
  mbActions <- get (C8.pack "actions")
  return $
    case mbActions of
      Just actions -> actions
      _ -> []

-- | Put actions to the local mutable state
putActions :: (B.Binary a) => [Action a] -> N2O f a ()
putActions = put (C8.pack "actions")
