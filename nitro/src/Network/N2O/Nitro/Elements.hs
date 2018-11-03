{-# LANGUAGE FlexibleInstances, OverloadedStrings, CPP #-}
module Network.N2O.Nitro.Elements where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Network.N2O.Nitro as Nitro
import Prelude hiding (id)

data Option a = Option {} deriving Show

#define ELEMENT_BASE()\
     id          :: BS.ByteString\
   , validation  :: BS.ByteString,             validate    :: BS.ByteString\
   , class_      :: [BS.ByteString],           style       :: BS.ByteString\
   , source      :: [BS.ByteString],           onClick     :: BS.ByteString\
   , onMouseOver :: BS.ByteString,             onKeyPress  :: BS.ByteString\
   , onBlur      :: BS.ByteString,             onChange    :: BS.ByteString\
   , onKeyUp     :: BS.ByteString,             onKeyDown   :: BS.ByteString\
   , onFocus     :: BS.ByteString,             dataFields  :: [(BS.ByteString, BS.ByteString)]\
   , body        :: [Element a],               role        :: BS.ByteString\
   , tabIndex    :: Integer,                   htmlTag     :: BS.ByteString\
   , title       :: T.Text,                    postback    :: Maybe a

#define ELEMENT_BASE_DEFAULTS()\
   id="",class_=[],style="",postback=Nothing,body=[],dataFields=[]\
  ,onFocus="",onBlur="",onChange="",onClick="",onKeyDown="",onKeyUp="",onKeyPress="",onMouseOver=""\
  ,tabIndex=0,validation="",validate="",source=[],role="",title="",htmlTag=undefined

#define CONTROL_BASE()\
  ELEMENT_BASE()

data Element a =
   MkBase
 { ELEMENT_BASE() }
 | MkLiter
 { ELEMENT_BASE(), htmlEncode::Bool, text::TL.Text }
 | MkList
 { ELEMENT_BASE(), ordered::Bool }
 | MkDropDown
 { ELEMENT_BASE(), options::[Option a], value::BS.ByteString, multiple::Bool, disabled::Bool, name::BS.ByteString}
 | MkSpinner
 { ELEMENT_BASE(), image::BS.ByteString }
 | MkMetaLink
 { ELEMENT_BASE(), href::BS.ByteString, hreflang::BS.ByteString, media::BS.ByteString
 , rel::BS.ByteString, sizes::BS.ByteString, type_::BS.ByteString }
 | MkMeta
 { ELEMENT_BASE(), charset::BS.ByteString, content::BS.ByteString, httpEquiv::BS.ByteString, name::BS.ByteString
 , type_::BS.ByteString }
 deriving (Show)

base :: Element a
base = MkBase{ELEMENT_BASE_DEFAULTS()}

literal :: Element a
literal = MkLiter{ELEMENT_BASE_DEFAULTS(),htmlEncode=True,text=""}

panel :: Element a
panel = base{htmlTag="div"}

list :: Element a
list = MkList{ELEMENT_BASE_DEFAULTS(),ordered=False}

dropDown :: Element a
dropDown = MkDropDown{ELEMENT_BASE_DEFAULTS(),options=[],value="",multiple=False,disabled=False,name=""}

radioGroup :: Element a
radioGroup = base

spinner :: Element a
spinner = MkSpinner{ELEMENT_BASE_DEFAULTS(),image="/priv/static/spinner.gif"}

head :: Element a
head = base

metaLink :: Element a
metaLink = MkMetaLink{ELEMENT_BASE_DEFAULTS(),href="",hreflang="",media="",rel="",sizes="",type_=""}
