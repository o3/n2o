module Network.N2O.Util where

import Numeric (showHex)
import Data.Char (isAlphaNum, ord)
import Data.Coerce (coerce)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as C8

jsEscape :: C8.ByteString -> C8.ByteString
jsEscape t = encodeUtf8 $ T.pack (escape (T.unpack $ decodeUtf8 t) "")
  where
    escape "" acc = acc
    escape (x : xs) acc = escape xs $
      if isAlphaNum x then
        acc ++ [x]
      else
        acc <> "\\x" <> (flip showHex "" . ord $ x)
