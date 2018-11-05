{-# LANGUAGE FlexibleInstances, DeriveGeneric, OverloadedStrings, CPP #-}
module Web.Nitro.Elements (Element(..)) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8

#include "nitro.h"

type Renderer a = Element a -> BL.ByteString
instance Show (Renderer a) where
  show _ = "Renderer"

data Element a =
   MkBase
 { ELEMENT_BASE() }
 | MkLiter
 { ELEMENT_BASE(), htmlEncode::Bool, text::TL.Text }
 | MkList
 { ELEMENT_BASE(), ordered::Bool }
 | MkDropDown
 { ELEMENT_BASE(), options::[Element a], value::BS.ByteString, multiple::Bool, disabled::Bool, name::BS.ByteString}
 | MkSpinner
 { ELEMENT_BASE(), image::BS.ByteString }
 | MkMetaLink
 { ELEMENT_BASE(), href::BS.ByteString, hreflang::BS.ByteString, media::BS.ByteString
 , rel::BS.ByteString, sizes::BS.ByteString, type_::BS.ByteString }
 | MkMeta
 { ELEMENT_BASE(), charset::BS.ByteString, content::BS.ByteString, httpEquiv::BS.ByteString, name::BS.ByteString
 , type_::BS.ByteString }
 | MkStyle
 { ELEMENT_BASE(), media::BS.ByteString, scoped::BS.ByteString, type_::BS.ByteString }
 | MkEdit
 { ELEMENT_BASE(), cite::BS.ByteString, datetime::BS.ByteString }
 | MkArea
 { ELEMENT_BASE(), alt::T.Text, coords::BS.ByteString, href::BS.ByteString, hreflang::BS.ByteString
 , media::BS.ByteString, target::BS.ByteString, rel::BS.ByteString, shape::BS.ByteString, type_::BS.ByteString}
 | MkAudio
 { ELEMENT_BASE(), autoplay::Bool, controls::BS.ByteString, loop::BS.ByteString, mediagroup::BS.ByteString
 , muted::BS.ByteString, preload::BS.ByteString, src::BS.ByteString, width::BS.ByteString }
 | MkCanvas
 { ELEMENT_BASE(), height::BS.ByteString, width::BS.ByteString }
 | MkEmbed
 { ELEMENT_BASE(), height::BS.ByteString, width::BS.ByteString, src::BS.ByteString, type_::BS.ByteString }
 | MkIframe
 { ELEMENT_BASE(), height::BS.ByteString, width::BS.ByteString, name::BS.ByteString, sandbox::BS.ByteString
 , seamless::BS.ByteString, src::BS.ByteString, srcdoc::BS.ByteString }
 | MkImage
 { ELEMENT_BASE(), alt::T.Text, height::BS.ByteString, ismap::BS.ByteString, src::BS.ByteString, usemap::Bool
 , width::BS.ByteString, image::BS.ByteString }
 | MkMap
 { ELEMENT_BASE(), name::BS.ByteString }
 | MkObject
 { ELEMENT_BASE(), data_::BS.ByteString, form::BS.ByteString, height::BS.ByteString, name::BS.ByteString
 , type_::BS.ByteString, usemap::Bool, width::BS.ByteString }
 | MkParam
 { ELEMENT_BASE(), name::BS.ByteString, value::BS.ByteString }
 | MkSource
 { ELEMENT_BASE(), media::BS.ByteString, src::BS.ByteString, type_::BS.ByteString }
 | MkTrack
 { ELEMENT_BASE(), default_::BS.ByteString, kind::BS.ByteString, label::BS.ByteString, src::BS.ByteString
 , srclang::BS.ByteString }
 | MkVideo
 { ELEMENT_BASE(), autoplay::Bool, controls::BS.ByteString, height::BS.ByteString, loop::BS.ByteString
 , width::BS.ByteString, mediagroup::BS.ByteString, muted::BS.ByteString, poster::BS.ByteString
 , preload::BS.ByteString, src::BS.ByteString}
 | MkButton
 { ELEMENT_BASE(), autofocus::Bool, disabled::Bool, form::BS.ByteString, formaction::BS.ByteString
 , formenctype::BS.ByteString, formmethod::BS.ByteString, formtarget::BS.ByteString, formnovalidate::Bool
 , name::BS.ByteString, type_::BS.ByteString, value::BS.ByteString }
 | MkFielset
 { ELEMENT_BASE(), disabled::Bool, form::BS.ByteString, name::BS.ByteString, legend::BS.ByteString }
 | MkForm
 { ELEMENT_BASE(), accept_charset::BS.ByteString, action::BS.ByteString, autocomplete::Bool, enctype::BS.ByteString
 , method::BS.ByteString, name::BS.ByteString, novalidate::Bool, target::BS.ByteString }
 | MkKeygen
 { ELEMENT_BASE(), autofocus::Bool, challenge::BS.ByteString, disabled::Bool, form::BS.ByteString
 , keytype::BS.ByteString, name::BS.ByteString }
 | MkLabel
 { ELEMENT_BASE(), for::BS.ByteString, form::BS.ByteString }
 | MkMeter
 { ELEMENT_BASE(), high::BS.ByteString, low::BS.ByteString, max_::BS.ByteString, min_::BS.ByteString
 , optimum::BS.ByteString, value::BS.ByteString }
 | MkOptgroup
 { ELEMENT_BASE(), disabled::Bool, label::BS.ByteString }
 | MkOption
 { ELEMENT_BASE(), disabled::Bool, label::BS.ByteString, selected::Bool, value::BS.ByteString }
 | MkOutput
 { ELEMENT_BASE(), for::BS.ByteString, form::BS.ByteString, name::BS.ByteString }
 | MkProgress
 { ELEMENT_BASE(), max_::BS.ByteString, value::BS.ByteString }
 | MkSelect
 { ELEMENT_BASE(), autofocus::Bool, disabled::Bool, form::BS.ByteString, multiple::Bool, name::BS.ByteString
 , required::Bool, size::BS.ByteString}
 | MkTextarea
 { ELEMENT_BASE(), autofocus::Bool, cols::BS.ByteString, dirname::BS.ByteString, disabled::Bool, form::BS.ByteString
 , maxlength::BS.ByteString, name::BS.ByteString, placeholder::BS.ByteString, readonly::Bool, required::Bool
 , rows::BS.ByteString, wrap::BS.ByteString, value::BS.ByteString }
 deriving (Show)
