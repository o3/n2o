{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns, RecordWildCards #-}

module Web.Nitro.Internal where

import Control.Monad (forM_, void)
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding as T
import Data.Char (isAlphaNum, ord, toLower)
import Data.IORef
import Numeric (showHex)
import Network.N2O hiding (Event)
import Web.Nitro.Elements (Element(..))

infixr 5 :<
pattern b :< bs  <- (TL.uncons -> Just (b, bs))
pattern TEmpty   <- (TL.uncons -> Nothing)

-- | Escape untrusted text to prevent XSS
jsEscape :: CL8.ByteString -> TL.Text
jsEscape = jsEscapeT . TL.decodeUtf8

-- | Escape untrusted text to prevent XSS
jsEscapeT :: TL.Text -> TL.Text
jsEscapeT t = escape t TL.empty
  where
    escape TEmpty acc = acc
    escape (x :< xs) acc = escape xs $
      if isAlphaNum x then
        TL.snoc acc x
      else
        acc <> "\\x" <> TL.pack (flip showHex "" . ord $ x)

htmlEscape :: TL.Text -> TL.Text
htmlEscape t = escape t TL.empty
  where
    escape TEmpty acc = acc
    escape (x :< xs) acc = escape xs $ acc <> escapeChar x
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar '/' = "&#x2F;"
    escapeChar x = TL.singleton x

htmlEscapeAggressive :: TL.Text -> TL.Text
htmlEscapeAggressive t = escape t TL.empty
  where
    escape TEmpty acc = acc
    escape (x :< xs) acc = escape xs $ acc <> escapeChar x
    escapeChar x
      | isAlphaNum x || ord x > 255 = TL.singleton x
      | otherwise = "&#x" <> TL.pack (flip showHex "" . ord $ x) <> ";"


-- | Action that can be rendered as JavaScript events
data Action a
  = AEvent (Event a)
  | AElement (Element a)
  | ARaw BL.ByteString
  deriving (Show)

-- | Custom data type
type NitroPlugin a = [Action a]

-- | A JavaScript event
data Event a = Event
  { eventTarget   :: BS.ByteString
  , eventPostback :: a
  , eventType     :: BS.ByteString
  , eventSource   :: [BS.ByteString]
  } deriving (Show)

-- | Wire an element
wireEl :: Element a -> N2O f a (NitroPlugin a) (Result a)
wireEl = wire . AElement

-- | Wire action
wire :: Action a -> N2O f a (NitroPlugin a) (Result a)
wire a = do
  actions <- getActions
  putActions (a : actions)
  return Empty

-- | Render list of actions to JavaScript
renderActions :: [Action a] -> N2O f a (NitroPlugin a) BL.ByteString
renderActions [] = return ""
renderActions (a:as) = do
  r <- renderAction a
  rs <- renderActions as
  return (r <> ";" <> rs)

-- | Render an action
renderAction :: Action a -> N2O f a (NitroPlugin a) BL.ByteString
renderAction (ARaw bs) = return bs
renderAction (AEvent ev) = renderEvent ev
renderAction (AElement el) = do
  case postback el of
    Nothing -> return ()
    Just pb -> void (wire $ AEvent Event
        {eventType = "click", eventPostback = pb, eventTarget = id_ el, eventSource = source el})
  return ""

-- | Render list of elements to the HTML
renderElements :: [Element a] -> N2O f a (NitroPlugin a) BL.ByteString
renderElements [] = return ""
renderElements (e:es) = do
  r <- renderElement e
  rs <- renderElements es
  return (r <> rs)

-- | Render element to the HTML
renderElement :: Element a -> N2O f a (NitroPlugin a) BL.ByteString
renderElement el = return $ renderer el el

-- | Render event
renderEvent :: Event a -> N2O f a (NitroPlugin a) BL.ByteString
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
updateText :: BS.ByteString -> TL.Text -> N2O f a (NitroPlugin a) (Result a)
updateText target s = wire
  (ARaw ("qi('" <> BL.fromStrict target <> "').innerText='"
         <> TL.encodeUtf8 s <> "'"))

insertBottom :: BS.ByteString -> Element a -> N2O f a (NitroPlugin a) (Result a)
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
getActions :: N2O f a (NitroPlugin a) [Action a]
getActions = do
  cx <- getContext
  let mbActions = cxCustom cx
  return $
    case mbActions of
      Just actions -> actions
      _ -> []

-- | Put actions to the local mutable state
putActions :: [Action a] -> N2O f a (NitroPlugin a) ()
putActions actions = do
  ref <- ask
  lift $ modifyIORef ref (\cx -> cx{cxCustom = Just actions})
