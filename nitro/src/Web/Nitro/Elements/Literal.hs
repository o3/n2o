{-# LANGUAGE CPP, OverloadedStrings #-}
module Web.Nitro.Elements.Literal where

import Web.Nitro.Internal
import Web.Nitro.Elements
import Web.Nitro.Tags
import Data.Text.Lazy.Encoding as TL

#include "nitro.h"

literal :: Element a
literal = MkLiter {htmlEncode = True, text = "", renderer = render}
  where
    render MkLiter {htmlEncode = htmlEncode, text = text} =
      TL.encodeUtf8 $
      if htmlEncode
        then htmlEscape text
        else text