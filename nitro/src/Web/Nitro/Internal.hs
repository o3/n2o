{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Web.Nitro.Internal where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class
import qualified Data.Serialize as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Map.Strict((!?))
import Network.N2O hiding (Event)
import Web.Nitro.Elements (Element(..))
import Web.Nitro.Elements.Render (render)
import Web.ClientSession

-- | Top level sum of protocols
data N2OProto a
  = N2ONitro (Nitro a)
  | Io BS.ByteString
       BS.ByteString
  | Nop
  deriving (Show)

-- | Nitro protocol message type
data Nitro a
  = NitroInit BS.ByteString
  | NitroPickle { pickleSource :: BS.ByteString
                , picklePickled :: BS.ByteString
                , pickleLinked :: M.Map BS.ByteString BS.ByteString }
  | NitroDone
  deriving (Show)

type StateRef a = IORef (State a)

data State a = MkState
  { stActions :: [Action a]
  , stContext :: Context N2OProto a (StateRef a)
  , stDict    :: M.Map BS.ByteString BS.ByteString }

-- | Action that can be rendered as JavaScript events
data Action a
  = AEvent (Event a)
  | AElement (Element a)
  | ARaw BS.ByteString
  deriving (Show)

-- | A JavaScript event
data Event a = Event
  { eventTarget   :: BS.ByteString
  , eventPostback :: a
  , eventType     :: BS.ByteString
  , eventSource   :: [BS.ByteString]
  } deriving (Show)

-- | Wire an element
wireEl :: Element a -> N2O (StateRef a) (Result a)
wireEl = wire . AElement

-- | Wire action
wire :: Action a -> N2O (StateRef a) (Result a)
wire a = do
  actions <- getActions
  putActions (a : actions)
  return Empty

-- | Render list of actions to JavaScript
renderActions :: [Action a] -> N2O (StateRef a) BS.ByteString
renderActions [] = return ""
renderActions (a:as) = do
  r <- renderAction a
  rs <- renderActions as
  return (r <> ";" <> rs)

-- | Render an action
renderAction :: Action a -> N2O (StateRef a) BS.ByteString
renderAction (ARaw bs) = return bs
renderAction (AEvent ev) = renderEvent ev
renderAction (AElement el) = do
  case postback el of
    Nothing -> return ()
    Just pb -> void (wire $ AEvent Event
        {eventType = "click", eventPostback = pb, eventTarget = id_ el, eventSource = source el})
  return ""

-- | Render list of elements to the HTML
renderElements :: [Element a] -> N2O (StateRef a) BS.ByteString
renderElements [] = return ""
renderElements (e:es) = do
  r <- renderElement e
  rs <- renderElements es
  return (r <> rs)

-- | Render element to the HTML
renderElement :: Element a -> N2O (StateRef a) BS.ByteString
renderElement = (fmap render) . return

-- | Render event
renderEvent :: Event a -> N2O (StateRef a) BS.ByteString
renderEvent Event {..} = do
  ref <- ask
  Context{cxPickle=pickle} <- getContext
  pickled <- pickle eventPostback
  case eventSource of
    [] -> return BS.empty
    src ->
      return $
      "{ var x=qi('" <> eventTarget <> "'); x && x.addEventListener('" <> eventType <>
      "',function(event){ if (validateSources([" <> strJoin (map (\x -> "'" <> x <> "'") src) <>
      "])) { ws.send(enc(tuple(atom('pickle'),bin('" <> eventTarget <>
      "'),bin('" <> pickled <> "'),[" <> strJoin (map renderSource src) <>
      "]))); } else console.log('Validation error'); })}"
  where
    renderSource :: BS.ByteString -> BS.ByteString
    renderSource s = "tuple(atom('" <> s <> "'),querySource('" <> s <> "'))"
    strJoin :: [BS.ByteString] -> BS.ByteString
    strJoin = BS.intercalate ","

-- | Update text content of the element with the specified @id@
updateText :: BS.ByteString -> T.Text -> N2O (StateRef a) (Result a)
updateText target s = wire
  (ARaw ("qi('" <> target <> "').innerText='"
         <> T.encodeUtf8 s <> "'"))

insertBottom :: BS.ByteString -> Element a -> N2O (StateRef a) (Result a)
insertBottom target elem = do
  content <- renderElement elem
  let action =
        "(function(){ var div = qn('div'); div.innerHTML = '" <>
        T.decodeUtf8 content <> "';qi('" <>
        T.decodeUtf8 target <>
        "').appendChild(div.firstChild); })();"
  wire $ ARaw $ T.encodeUtf8 action

-- | Default pickler
defPickle :: (B.Serialize a, MonadIO m) => a -> m BS.ByteString
defPickle a =
  let bs = B.encode a in
  liftIO $ getDefaultKey >>= \key ->
  liftIO $ encryptIO key bs

-- | Default depickler
defDePickle :: (B.Serialize a, MonadIO m) => BS.ByteString -> m (Maybe a)
defDePickle bs =
  liftIO $ getDefaultKey >>= \key ->
  case decrypt key bs of
    Just bin -> case B.decode $ bin of
                  Right x -> return $ Just x
                  _ -> return Nothing
    _ -> return Nothing

-- | Get action list from the local mutable state
getActions :: N2O (StateRef a) [Action a]
getActions = do
  ref <- ask
  st <- liftIO $ readIORef ref
  return $ stActions st

-- | Put actions to the local mutable state
putActions :: [Action a] -> N2O (StateRef a) ()
putActions actions = do
  ref <- ask
  liftIO $ modifyIORef ref (\st@MkState{} -> st{stActions=actions})

-- | Put data to the local state
put :: (B.Serialize bin) => BS.ByteString -> bin -> N2O (StateRef a) ()
put k v = do
  ref <- ask
  liftIO $ modifyIORef ref (\st@MkState{stDict=dict} -> st{stDict=(M.insert k (B.encode v) dict)})

-- | Get data from the local state
get :: (B.Serialize bin) => BS.ByteString -> N2O (StateRef a) (Maybe bin)
get k = do
  ref <- ReaderT return
  st <- liftIO $ readIORef ref
  let m = stDict st
  case m !? k of
    Just v -> case (B.decode v) of
                Right x -> return $ Just x
                _ -> return Nothing
    _ -> return Nothing

getContext :: N2O (StateRef a) (Context N2OProto a (IORef (State a)))
getContext = do
  ref <- ReaderT return
  st <- liftIO $ readIORef ref
  return $ stContext st
