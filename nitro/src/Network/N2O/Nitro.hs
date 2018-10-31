{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric,
  ScopedTypeVariables #-}

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
import Data.Char (isAlphaNum, ord, toLower)
import Data.IORef
import Data.List (intercalate)
import Data.Map.Strict ((!?))
import Data.String
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Fmt
import Fmt.Internal.Core
import GHC.Generics (Generic)
import Network.N2O hiding (Event)
import Numeric (showHex)
import Prelude hiding (id)

-- | An HTML element
data Element a
  = Element { name      :: String
            , id        :: String
            , body      :: [Element a]
            , postback  :: Maybe a
            , source    :: [String]
            , noBody    :: Bool
            , noClosing :: Bool
            }
  | Text TL.Text
  deriving (Show, Generic)

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
  { eventTarget   :: String
  , eventPostback :: a
  , eventType     :: String
  , eventSource   :: [String]
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
renderAction (AElement el@Element {..}) = do
  case postback of
    Nothing -> return ()
    Just pb -> void (wire $ AEvent Event
        {eventType = "click", eventPostback = pb, eventTarget = id, eventSource = source})
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
renderElement (Text t) = return $ encodeUtf8 t
renderElement Element {..} = do
  case postback of
    Nothing -> return ()
    Just pb -> void (wire $ AEvent Event
      {eventType = "click", eventPostback = pb, eventTarget = id, eventSource = source})
  case name of
    "br" -> return "<br>"
    _ -> do
      content <- renderElements body
      return $
        encodeUtf8 $
        if noBody
          then "<" +| name |+ " " +| idProp id |+ "/>"
          else "<" +| name |+ " " +| idProp id |+ ">" +| decodeUtf8 content |+ "</" +| name |+ ">"
  where
    idProp :: String -> String
    idProp x =
      if x == ""
        then ""
        else "id=\"" +| x |+ "\""

-- | Render event
renderEvent :: Event a -> N2O f a BL.ByteString
renderEvent Event {..} = do
  ref <- ask
  cx@Context {cxPickle = pickle} <- lift $ readIORef ref
  case eventSource of
    [] -> return BL.empty
    src -> return $ encodeUtf8 $
            "{ var x=qi('" +| eventTarget |+ "'); x && x.addEventListener('"
            +| eventType |+ "',function(event){ if (validateSources("
            +| strJoin (map (\x -> "'" ++ x ++  "'") eventSource) |+
            ")) { ws.send(enc(tuple(atom('pickle'),bin('" +| eventTarget |+ "'),bin('"
            +| decodeUtf8 (pickle eventPostback) |+ "'),"
            +| strJoin (map renderSource src) |+
            "))); } else console.log('Validation error'); })}"
  where
    renderSource s = "tuple(atom('" +| s |+ "'),querySource('" +| s |+ "'))"
    strJoin [] = "[]"
    strJoin l = "[" ++ intercalate "," l ++ "]"

-- | Element constructor
baseElement :: Element a
baseElement =
  Element
    { id = ""
    , name = undefined
    , postback = Nothing
    , body = []
    , source = []
    , noBody = False
    , noClosing = False
    }

-- | An HTML button
button :: Element a
button = baseElement {name = "button", source = []}

-- | A @panel@ widget
panel = baseElement {name = "div"}

-- | Text node
text :: TL.Text -> Element a
text = Text

-- | @<br>@ element
br = baseElement {name = "br", noBody = True, noClosing = True}

-- | A @textbox@ widget
textbox :: Element a
textbox = baseElement {name = "input type=\"text\"", noBody = True}

-- | Update text content of the element with the specified @id@
updateText :: (B.Binary a) => BL.ByteString -> TL.Text -> N2O f a (Result a)
updateText target s = wire
  (ARaw $ encodeUtf8 ("qi('" +| decodeUtf8 target |+ "').innerText='" +| s |+ "'"))

{-insertBottom :: Nitro m => String -> Element -> m ()
insertBottom target elem = do
  content <- renderElement elem
  acs <- actions
  let tag = "div" :: String
      action =
        "(function(){ var div = qn('" +| tag |+ "'); div.innerHTML = '" +|
        content |+ "';qi('" +| target |+ "').appendChild(div.firstChild); })();"
  
  clear
  wire $ T.pack action
  forM_ acs wire-}

-- | Escape untrusted text to prevent XSS
jsEscapeT :: TL.Text -> TL.Text
jsEscapeT t = TL.pack (escape (TL.unpack t) "")
  where
    escape "" acc = acc
    escape (x:xs) acc =
      escape xs $
      if isAlphaNum x
        then acc ++ [x]
        else acc <> "\\x" <> (flip showHex "" . ord $ x)

-- | Escape untrusted text to prevent XSS
jsEscape :: CL8.ByteString -> TL.Text
jsEscape = jsEscapeT . decodeUtf8

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
