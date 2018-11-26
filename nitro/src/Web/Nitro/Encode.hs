{-# LANGUAGE PatternSynonyms, ViewPatterns, OverloadedStrings #-}

module Web.Nitro.Encode where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.Serialize as B
import Data.Char (isAlphaNum, ord, toLower)
import Numeric (showHex)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
