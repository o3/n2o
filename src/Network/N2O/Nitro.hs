{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.N2O.Nitro where

import qualified Data.ByteString             as BS
import           Data.ByteString.Base64.Lazy (decode, encode)
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as C8
import           Data.Char                   (toLower)
import           Data.List                   (intersperse)
import           Data.String
import qualified Data.Text                   as T
import           Fmt
import           Fmt.Internal.Core
import           Prelude                     hiding (id)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_)

type Action = T.Text

data Element
  = Element { name      :: String
            , id        :: String
            , body      :: [Element]
            , postback  :: String
            , source    :: [String]
            , noBody    :: Bool
            , noClosing :: Bool
            }
  | Text T.Text
  deriving (Show)

-- data EventType = Click deriving (Show)
data Event = Event
  { eventTarget   :: String
  , eventPostback :: String
  , eventType     :: String
  , eventSource   :: [String]
  } deriving (Show)

class (MonadIO m) => Nitro m where
  wire :: Action -> m () -- wire an action
  actions :: m [Action] -- get actions
  clear :: m () -- clear actions

-- pickle :: Term -> String
-- pickle =  C8.unpack . encode . Bin.encode

-- depickle :: BL.ByteString -> Term
-- depickle b = case decode b of
--   Left e   -> error e
--   Right bs -> Bin.decode bs

-- Erlang N2O polymorphism of render function:
-- each record has 'module' field - it is the dispatch value
-- nitro:render/1 - this is interface
-- wf_render:render/1 - dispatcher function
-- action_event:render_action/1 - implementation for events
-- format: {pickle,target,event,_,{name1,val1},{name2,val2}...}
renderEvent :: (Monad m) => Event -> m Action
renderEvent Event {..} =
      case eventPostback of
        "" -> return ""
        pb ->
          return $
            "{ var x=qi('" +| eventTarget |+ "'); x && x.addEventListener('"
            +| eventType |+ "',function(event){ if (validateSources(["
            +| (strJoin $ map (\x -> "'" ++ x ++  "'") eventSource) |+
            "])) { ws.send(data('" +| pb |+"',["
            +| (strJoin $ map renderSource eventSource) |+
            "])); } else console.log('Validation error'); })};"
  where
    renderSource s = "querySource2('" +| s |+"')"
    strJoin [] = ""
    strJoin l  = (concat $ intersperse "," l)

renderElements :: Nitro m => [Element] -> m Action
renderElements []     = return ""
renderElements (x:xs) = do
  y <- renderElement x
  ys <- renderElements xs
  return $ y <> ys

renderElement :: Nitro m => Element -> m Action
renderElement (Text t) = return t
renderElement Element {..} = do
  case postback of
    "" -> return ()
    pb -> renderEvent click{eventPostback=pb,eventTarget=id,eventSource=source}
        >>= wire
  case name of
    "br" -> return "<br>"
    _ -> do
      content <- renderElements body
      return $
        T.pack $
        if noBody
          then "<" +| name |+ " " +| (idProp id) |+ "/>"
          else "<" +| name |+ " " +| (idProp id) |+ ">" +| content |+ "</" +| name |+
               ">"
  where
    idProp :: String -> String
    idProp x =
      if x == ""
        then ""
        else "id=\"" +| x |+ "\""

baseElement :: Element
baseElement =
  Element
    { id = ""
    , name = undefined
    , postback = ""
    , body = []
    , source = []
    , noBody = False
    , noClosing = False
    }

button :: Element
button = baseElement { name = "button"
                     , source = [] }

panel = baseElement { name = "div" }

text :: T.Text -> Element
text s = Text s

br = baseElement {name = "br", noBody = True, noClosing = True}

textbox :: Element
textbox = baseElement {name = "input type=\"text\"", noBody = True}

alert :: Nitro m => T.Text -> m () 
alert s = wire $ "alert('" <> s <> "');"

insertBottom :: Nitro m => String -> Element -> m ()
insertBottom target elem = do
  content <- renderElement elem
  acs <- actions
  let tag = "div" :: String
      action =
        "(function(){ var div = qn('" +| tag |+ "'); div.innerHTML = '" +|
        content |+ "';qi('" +| target |+ "').appendChild(div.firstChild); })();"
  
  clear
  wire $ T.pack action
  forM_ acs wire

event =
  Event
    { eventTarget = ""
    , eventPostback = ""
    , eventType = undefined
    , eventSource = []
    }

click = event {eventType = "click"}

