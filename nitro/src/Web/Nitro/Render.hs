{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Nitro.Render where

import Control.Monad.IO.Class (MonadIO)
import Web.Nitro.Types (Event(..), Element(..))
import Web.Nitro.Tags (emitTag)
import Web.Nitro.Encode (htmlEscape, pickle)
import qualified Data.Serialize as B
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

class Renderable r where
  render_ :: (MonadIO m) => r -> m (BS.ByteString, BS.ByteString)
instance Renderable BS.ByteString where
  render_ b = return ("", b)

instance (B.Serialize a) => Renderable (Event a) where
  render_ Event{..} = do
    pickled <- pickle (eventPostback :: a)
    return $ ("",
              "{ var x=qi('" <> eventTarget <> "'); x && x.addEventListener('" <> eventType <>
              "',function(event){ if (validateSources([" <> strJoin (map (\x -> "'" <> x <> "'") eventSource) <>
              "])) { ws.send(enc(tuple(atom('pickle'),bin('" <> eventTarget <>
              "'),bin('" <> pickled <> "'),[" <> strJoin (map renderSource eventSource) <>
              "]))); } else console.log('Validation error'); })}")
      where
        renderSource :: BS.ByteString -> BS.ByteString
        renderSource s = "tuple(atom('" <> s <> "'),querySource('" <> s <> "'))"
        strJoin :: [BS.ByteString] -> BS.ByteString
        strJoin = BS.intercalate ","

-- | Render list of elements to the HTML + JS
instance (B.Serialize a) => Renderable [Element a] where
  render_ [] = return ("","")
  render_ (e:es) = do
    r <- render_ e
    rs <- render_ es
    return (r <> rs)

-- | Render element to the HTML + JS
instance (B.Serialize a) => Renderable (Element a) where
  render_ el@MkList{} = do
    (content, js) <- render_ (body el)
    let tag = if ordered el then "ol" else "ul"
        html = emitTag tag content (defaultProps el)
    return (html, js)
  render_ el@MkTextarea{} = do
    (content, _) <- render_ (body el)
    let html = emitTag "textarea" content
               ([ ("autofocus", if autofocus el then "autofocus" else "")
                , ("cols", cols el), ("dirname", dirname el)
                , ("disabled", if disabled el then "disabled" else "")
                , ("form", form el), ("maxlength", maxlength el)
                , ("name", name el), ("placeholder", placeholder el)
                , ("readonly", if readonly el then "readonly" else "")
                , ("required", if readonly el then "required" else "")
                , ("rows", rows el), ("wrap", wrap el)
                ] <> defaultProps el <> dataFields el)
    return (html, "")
  render_ MkLiter {htmlEncode = htmlEncode, text = text} =
    return (T.encodeUtf8 $ if htmlEncode then htmlEscape text else text, "")
  render_ el@MkButton{} = do
    ("", ev) <- case postback el of
      Just pb -> render_ Event{ eventType="click"
                              , eventPostback=pb
                              , eventTarget=id_ el
                              , eventSource=source el }
      Nothing -> return ("","")
    (content, js) <- render_ (body el)
    let html =  emitTag "button" content
          ([("id",      id_ el),       ("class",    BS.intercalate " " (class_ el))
           ,("style",    style el),     ("name",     name el)
           ,("onchange", onchange el),  ("type",     type_ el)
           ,("onclick",  onclick el),   ("disabled", if disabled el then "disabled" else "")
           ,("value",    value el)
         ] <> dataFields el)
    return (html, js <> ev)
  render_ el = do
    (content, js) <- render_ (body el)
    let html = emitTag (htmlTag el) content (defaultProps el <> dataFields el)
    return (html, js)

defaultProps el = [ ("id",    id_ el),   ("class", BS.intercalate " " (class_ el))
                  , ("style", style el), ("title", T.encodeUtf8 $ title el)
                  , ("lang",  lang el),  ("tabindex", tabindex el)
                  , ("contenteditable",  if contenteditable el then "true" else "false") ]
