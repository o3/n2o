{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Nitro.Elements.Render where

import Control.Monad.IO.Class
import Web.Nitro.Elements
import Web.Nitro.Tags
import qualified Data.Serialize as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Char (isAlphaNum, ord, toLower)
import Numeric (showHex)
import Web.ClientSession

infixr 5 :<
pattern b :< bs  <- (T.uncons -> Just (b, bs))
pattern TEmpty   <- (T.uncons -> Nothing)

-- | Escape untrusted text to prevent XSS
jsEscape :: C8.ByteString -> T.Text
jsEscape = jsEscapeT . T.decodeUtf8

-- | Escape untrusted text to prevent XSS
jsEscapeT :: T.Text -> T.Text
jsEscapeT t = escape t T.empty
  where
    escape TEmpty acc = acc
    escape (x :< xs) acc = escape xs $
      if isAlphaNum x then
        T.snoc acc x
      else
        acc <> "\\x" <> T.pack (flip showHex "" . ord $ x)

htmlEscape :: T.Text -> T.Text
htmlEscape t = escape t T.empty
  where
    escape TEmpty acc = acc
    escape (x :< xs) acc = escape xs $ acc <> escapeChar x
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar '/' = "&#x2F;"
    escapeChar x = T.singleton x

htmlEscapeAggressive :: T.Text -> T.Text
htmlEscapeAggressive t = escape t T.empty
  where
    escape TEmpty acc = acc
    escape (x :< xs) acc = escape xs $ acc <> escapeChar x
    escapeChar x
      | isAlphaNum x || ord x > 255 = T.singleton x
      | otherwise = "&#x" <> T.pack (flip showHex "" . ord $ x) <> ";"

pickle :: (B.Serialize a, MonadIO m) => a -> m BS.ByteString
pickle a =
  let bs = B.encode a in
  liftIO $ getDefaultKey >>= \key ->
  liftIO $ encryptIO key bs

depickle :: (B.Serialize a, MonadIO m) => BS.ByteString -> m (Maybe a)
depickle bs =
  liftIO $ getDefaultKey >>= \key ->
  case decrypt key bs of
    Just bin -> case B.decode $ bin of
                  Right x -> return $ Just x
                  _ -> return Nothing
    _ -> return Nothing

class Renderable r where
  render_ :: (MonadIO m) => r -> m (BS.ByteString, BS.ByteString)
instance Renderable BS.ByteString where
  render_ b = return ("", b)

instance (B.Serialize a) => Renderable (Event a) where
 -- render_ :: Event a -> N2O N2OProto a BS.ByteString
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

-- | Render list of elements to the HTML
instance (B.Serialize a) => Renderable [Element a] where
  -- render_ :: [Element a] -> N2O N2OProto a BS.ByteString
  render_ [] = return ("","")
  render_ (e:es) = do
    r <- render_ e
    rs <- render_ es
    return (r <> rs)

-- | Render element to the HTML
instance (B.Serialize a) => Renderable (Element a) where
  -- render_ :: Element a -> N2O N2OProto a BS.ByteString
  render_ = render

-- | Render element
render :: (MonadIO m, B.Serialize a) => Element a -> m (BS.ByteString, BS.ByteString)
render el@MkList{} = do
  (content, js) <- render_ (body el)
  let tag = if ordered el then "ol" else "ul"
      html = emitTag tag content (defaultProps el)
  return (html, js)
render el@MkTextarea{} = do
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
render MkLiter {htmlEncode = htmlEncode, text = text} =
  return (T.encodeUtf8 $ if htmlEncode then htmlEscape text else text, "")
render el@MkButton{} = do
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
render el = do
  (content, js) <- render_ (body el)
  let html = emitTag (htmlTag el) content (defaultProps el <> dataFields el)
  return (html, js)

defaultProps el = [ ("id",    id_ el),   ("class", BS.intercalate " " (class_ el))
                  , ("style", style el), ("title", T.encodeUtf8 $ title el)
                  , ("lang",  lang el),  ("tabindex", tabindex el)
                  , ("contenteditable",  if contenteditable el then "true" else "false") ]
