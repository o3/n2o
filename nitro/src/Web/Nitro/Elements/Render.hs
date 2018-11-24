{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
module Web.Nitro.Elements.Render where

import Web.Nitro.Elements
import Web.Nitro.Tags
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Char (isAlphaNum, ord, toLower)
import Numeric (showHex)

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

-- | Render element
render :: Element a -> BL.ByteString
render el@MkList{} =
  let tag = if ordered el then "ol" else "ul"
      content_ = mconcat $ fmap render (body el) in
  emitTag tag content_ (defaultProps el)
render el@MkTextarea{} =
  let content_ = mconcat $ fmap render (body el) in
  emitTag "textarea" content_ ([ ("autofocus", if autofocus el then "autofocus" else "")
                               , ("cols", cols el), ("dirname", dirname el)
                               , ("disabled", if disabled el then "disabled" else "")
                               , ("form", form el), ("maxlength", maxlength el)
                               , ("name", name el), ("placeholder", placeholder el)
                               , ("readonly", if readonly el then "readonly" else "")
                               , ("required", if readonly el then "required" else "")
                               , ("rows", rows el), ("wrap", wrap el)
                               ] <> defaultProps el <> dataFields el)
render MkLiter {htmlEncode = htmlEncode, text = text} =
  TL.encodeUtf8 $
  if htmlEncode
    then htmlEscape text
    else text
render el@MkButton{} =
  let content_ = mconcat $ fmap render (body el) in
  emitTag "button" content_ ([("id",      id_ el),       ("class",    BS.intercalate " " (class_ el))
                            ,("style",    style el),     ("name",     name el)
                            ,("onchange", onchange el),  ("type",     type_ el)
                            ,("onclick",  onclick el),   ("disabled", if disabled el then "disabled" else "")
                            ,("value",    value el)
                            ] <> dataFields el)
render el =
  let content_ = mconcat $ fmap render (body el) in
  emitTag (htmlTag el) content_ (defaultProps el <> dataFields el)

defaultProps el = [ ("id",    id_ el),   ("class", BS.intercalate " " (class_ el))
                  , ("style", style el), ("title", T.encodeUtf8 $ title el)
                  , ("lang",  lang el),  ("tabindex", tabindex el)
                  , ("contenteditable",  if contenteditable el then "true" else "false") ]
