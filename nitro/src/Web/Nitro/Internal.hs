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
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map.Strict as M
import Data.Map.Strict((!?))
import Numeric (showHex)
import Network.N2O hiding (Event)
import Web.Nitro.Elements (Element(..))
import qualified Data.Vault.Lazy as V
import System.IO.Unsafe

type N2O = N2OT (IORef V.Vault) IO

actionsKey :: V.Key [Action a]
actionsKey = unsafePerformIO V.newKey
{-# NOINLINE actionsKey #-}

contextKey :: V.Key (Context f a m)
contextKey = unsafePerformIO V.newKey
{-# NOINLINE contextKey #-}

dictKey :: V.Key (M.Map BS.ByteString BL.ByteString)
dictKey = unsafePerformIO V.newKey
{-# NOINLINE dictKey #-}

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

-- | A JavaScript event
data Event a = Event
  { eventTarget   :: BS.ByteString
  , eventPostback :: a
  , eventType     :: BS.ByteString
  , eventSource   :: [BS.ByteString]
  } deriving (Show)

-- | Wire an element
wireEl :: Element a -> N2O (Result a)
wireEl = wire . AElement

-- | Wire action
wire :: Action a -> N2O (Result a)
wire a = do
  actions <- getActions
  putActions (a : actions)
  return Empty

-- | Render list of actions to JavaScript
renderActions :: [Action a] -> N2O BL.ByteString
renderActions [] = return ""
renderActions (a:as) = do
  r <- renderAction a
  rs <- renderActions as
  return (r <> ";" <> rs)

-- | Render an action
renderAction :: Action a -> N2O BL.ByteString
renderAction (ARaw bs) = return bs
renderAction (AEvent ev) = renderEvent ev
renderAction (AElement el) = do
  case postback el of
    Nothing -> return ()
    Just pb -> void (wire $ AEvent Event
        {eventType = "click", eventPostback = pb, eventTarget = id_ el, eventSource = source el})
  return ""

-- | Render list of elements to the HTML
renderElements :: [Element a] -> N2O BL.ByteString
renderElements [] = return ""
renderElements (e:es) = do
  r <- renderElement e
  rs <- renderElements es
  return (r <> rs)

-- | Render element to the HTML
renderElement :: Element a -> N2O BL.ByteString
renderElement el = return $ renderer el el

-- | Render event
renderEvent :: Event a -> N2O BL.ByteString
renderEvent Event {..} = do
  ref <- ask
  vault <- lift $ readIORef ref
  let Context{cxPickle=pickle} = fromJust $ V.lookup contextKey vault
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
updateText :: BS.ByteString -> TL.Text -> N2O (Result a)
updateText target s = wire
  (ARaw ("qi('" <> BL.fromStrict target <> "').innerText='"
         <> TL.encodeUtf8 s <> "'"))

insertBottom :: BS.ByteString -> Element a -> N2O (Result a)
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
getActions :: N2O [Action a]
getActions = do
  ref <- ask
  vault <- lift $ readIORef ref
  return $ fromMaybe [] (V.lookup actionsKey vault)

-- | Put actions to the local mutable state
putActions :: [Action a] -> N2O ()
putActions actions = do
  ref <- ask
  lift $ modifyIORef ref (V.insert actionsKey actions)

-- | Put data to the local state
put :: (B.Binary bin) => BS.ByteString -> bin -> N2O ()
put k v = do
  ref <- ask
  lift $ modifyIORef ref (V.adjust (M.insert k (B.encode v)) dictKey)

-- | Get data from the local state
get :: (B.Binary bin) => BS.ByteString -> N2O (Maybe bin)
get k = do
  ref <- N2OT return
  vault <- lift $ readIORef ref
  let m = fromJust $ V.lookup dictKey vault
  case m !? k of
    Just v -> return $ Just (B.decode v)
    _ -> return Nothing


getContext :: N2O (Context f a N2O)
getContext = do
  ref <- N2OT return
  vault <- lift $ readIORef ref
  return $ fromJust $ V.lookup contextKey vault