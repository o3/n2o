{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}

module Network.N2O.Nitro where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as C8
import           Data.Char                   (toLower)
import           Data.List                   (intersperse)
import           Data.String
import qualified Data.Text                   as T
import Data.Text.Lazy.Encoding
import Data.Map.Strict ((!?))
import Data.IORef
import           Fmt
import           Fmt.Internal.Core
import           Prelude                     hiding (id)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_)
import Network.N2O.Types
import GHC.Generics (Generic)
import qualified Data.Binary as B

{-
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
-}

data Action a = AEvent (Event a) | ARaw BL.ByteString deriving (Show, Generic)
instance (B.Binary a) => B.Binary (Action a)

-- data EventType = Click deriving (Show)
data Event a = Event
  { eventTarget   :: String
  , eventPostback :: a
  , eventType     :: String
  , eventSource   :: [String]
  } deriving (Show, Generic)
instance (B.Binary a) => B.Binary (Event a)

wire :: forall f a b. (B.Binary a) => Action a -> N2O f a b BL.ByteString
wire a = do
  mbActions <- get "actions"
  let (actions :: [Action a]) = case mbActions of
                  Just actions -> actions
                  _ -> []
  put "actions" (a:actions)
  return ""

renderActions :: [Action a] -> N2O f a b BL.ByteString
renderActions [] = return ""
renderActions (a:as) = do
  r <- renderAction a
  rs <- renderActions as
  return (r <> ";" <> rs)

renderAction :: Action a -> N2O f a b BL.ByteString
renderAction (ARaw bs) = return bs
renderAction (AEvent ev) = renderEvent ev

renderEvent :: Event a -> N2O f a b BL.ByteString
renderEvent Event {..} = do
    ref <- ask
    cx@Cx{cxPickle=pickle} <- lift $ readIORef ref
    case eventSource of
      [] -> return BL.empty
      src -> return $ encodeUtf8 $
            "{ var x=qi('" +| eventTarget |+ "'); x && x.addEventListener('"
            +| eventType |+ "',function(event){ if (validateSources("
            +| (strJoin $ map (\x -> "'" ++ x ++  "'") eventSource) |+
            ")) { ws.send(enc(tuple(atom('pickle'),bin('" +| eventTarget |+ "'),bin('"
            +| (decodeUtf8 $ pickle eventPostback) |+ "'),"
            +| (strJoin $ map renderSource src) |+
            "))); } else console.log('Validation error'); })}"
  where
    renderSource s = "tuple(atom('" +| s |+ "'),querySource('" +| s |+"'))"
    strJoin [] = "[]"
    strJoin l  = "[" ++ (concat $ intersperse "," l) ++ "]"

{-
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
-}
