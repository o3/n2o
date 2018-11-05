{-# LANGUAGE OverloadedStrings, CPP #-}
module Web.Nitro.Elements.Button where

import Web.Nitro.Elements
import Web.Nitro.Tags
import qualified Data.ByteString as BS

#include "nitro.h"

button:: Element a
button = MkButton{ELEMENT_BASE_DEFAULTS1(),autofocus=True,disabled=False,form="",formaction="",formmethod=""
                 ,formtarget="",formnovalidate=False,formenctype="",name="",type_="button",value="",renderer=render}
  where
    render el@MkButton{} =
      let content_ = mconcat $ fmap (renderer el) (body el) in
      emitTag "button" content_ ([("id",      id_ el),       ("class",    BS.intercalate " " (class_ el))
                                ,("style",    style el),     ("name",     name el)
                                ,("onchange", onchange el),  ("type",     type_ el)
                                ,("onclick",  onclick el),   ("disabled", if disabled el then "disabled" else "")
                                ,("value",    value el)
                                ] ++ dataFields el)