module Network.N2O.Util where

import Numeric (showHex)
import Data.Char (isAlphaNum, ord)
import Data.Coerce (coerce)
import qualified Data.ByteString.Lazy.Char8 as C8

jsEscape :: C8.ByteString -> C8.ByteString
jsEscape t = C8.pack (escape (C8.unpack t) "")
  where
    escape "" acc = acc
    escape (x : xs) acc = escape xs $
      if isAlphaNum x then
        acc ++ [x]
      else
        acc <> "\\x" <> (flip showHex "" . ord $ x)
