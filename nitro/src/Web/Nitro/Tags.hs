{-# LANGUAGE OverloadedStrings #-}
module Web.Nitro.Tags where

import qualified Data.ByteString as BS
import Prelude hiding (void)

void :: BS.ByteString -> Bool
void tag = tag == "br" || tag == "hr" || tag == "link" || tag == "img" || tag == "input"
        || tag == "meta" || tag == "param" || tag == "base" || tag == "area" || tag == "col"
        || tag == "command" || tag == "option" || tag == "keygen" || tag == "source"

displayProp :: (BS.ByteString,BS.ByteString) -> BS.ByteString
displayProp (_,"") = ""
displayProp (k,v) = " " <> k <> "=\"" <> v <> "\""

emitTag :: BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
emitTag tag "" props
 | void tag = "<" <> tag <> mconcat (fmap displayProp props) <> ">"
 | otherwise = "<" <> tag <> mconcat (fmap displayProp props) <> "/>"
emitTag tag content props = "<" <> tag <> mconcat (fmap displayProp props)
                         <> ">" <> content <> "</" <> tag <> ">"
