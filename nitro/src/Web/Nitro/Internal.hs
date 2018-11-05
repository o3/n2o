{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Web.Nitro.Internal where

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
