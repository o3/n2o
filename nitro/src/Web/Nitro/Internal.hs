{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Web.Nitro.Internal where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding as T
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map.Strict as M
import Data.Map.Strict((!?))
import Network.N2O hiding (Event)
import Web.Nitro.Elements (Element(..))
import Web.Nitro.Elements.Render (render)
import qualified Data.Vault.Lazy as V
import System.IO.Unsafe

-- | Top level sum of protocols
data N2OProto a
  = N2ONitro (Nitro a)
  | Io BL.ByteString
       BL.ByteString
  | Nop
  deriving (Show)

-- | Nitro protocol message type
data Nitro a
  = NitroInit BL.ByteString
  | NitroPickle { pickleSource :: BL.ByteString
           , picklePickled :: BL.ByteString
           , pickleLinked :: M.Map BS.ByteString BL.ByteString }
  | NitroDone
  deriving (Show)

type NITRO = N2O (IORef V.Vault)

actionsKey :: V.Key [Action a]
actionsKey = unsafePerformIO V.newKey
{-# NOINLINE actionsKey #-}

contextKey :: V.Key (Context N2OProto a (IORef V.Vault))
contextKey = unsafePerformIO V.newKey
{-# NOINLINE contextKey #-}

dictKey :: V.Key (M.Map BS.ByteString BL.ByteString)
dictKey = unsafePerformIO V.newKey
{-# NOINLINE dictKey #-}

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
wireEl :: Element a -> NITRO (Result a)
wireEl = wire . AElement

-- | Wire action
wire :: Action a -> NITRO (Result a)
wire a = do
  actions <- getActions
  putActions (a : actions)
  return Empty

-- | Render list of actions to JavaScript
renderActions :: [Action a] -> NITRO BL.ByteString
renderActions [] = return ""
renderActions (a:as) = do
  r <- renderAction a
  rs <- renderActions as
  return (r <> ";" <> rs)

-- | Render an action
renderAction :: Action a -> NITRO BL.ByteString
renderAction (ARaw bs) = return bs
renderAction (AEvent ev) = renderEvent ev
renderAction (AElement el) = do
  case postback el of
    Nothing -> return ()
    Just pb -> void (wire $ AEvent Event
        {eventType = "click", eventPostback = pb, eventTarget = id_ el, eventSource = source el})
  return ""

-- | Render list of elements to the HTML
renderElements :: [Element a] -> NITRO BL.ByteString
renderElements [] = return ""
renderElements (e:es) = do
  r <- renderElement e
  rs <- renderElements es
  return (r <> rs)

-- | Render element to the HTML
renderElement :: Element a -> NITRO BL.ByteString
renderElement = (fmap render) . return

-- | Render event
renderEvent :: Event a -> NITRO BL.ByteString
renderEvent Event {..} = do
  ref <- ask
  vault <- liftIO $ readIORef ref
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
updateText :: BS.ByteString -> TL.Text -> NITRO (Result a)
updateText target s = wire
  (ARaw ("qi('" <> BL.fromStrict target <> "').innerText='"
         <> TL.encodeUtf8 s <> "'"))

insertBottom :: BS.ByteString -> Element a -> NITRO (Result a)
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
getActions :: NITRO [Action a]
getActions = do
  ref <- ask
  vault <- liftIO $ readIORef ref
  return $ fromMaybe [] (V.lookup actionsKey vault)

-- | Put actions to the local mutable state
putActions :: [Action a] -> NITRO ()
putActions actions = do
  ref <- ask
  liftIO $ modifyIORef ref (V.insert actionsKey actions)

-- | Put data to the local state
put :: (B.Binary bin) => BS.ByteString -> bin -> NITRO ()
put k v = do
  ref <- ask
  liftIO $ modifyIORef ref (V.adjust (M.insert k (B.encode v)) dictKey)

-- | Get data from the local state
get :: (B.Binary bin) => BS.ByteString -> NITRO (Maybe bin)
get k = do
  ref <- ReaderT return
  vault <- liftIO $ readIORef ref
  let m = fromJust $ V.lookup dictKey vault
  case m !? k of
    Just v -> return $ Just (B.decode v)
    _ -> return Nothing

getContext :: NITRO (Context N2OProto a (IORef V.Vault))
getContext = do
  ref <- ReaderT return
  vault <- liftIO $ readIORef ref
  return $ fromJust $ V.lookup contextKey vault
