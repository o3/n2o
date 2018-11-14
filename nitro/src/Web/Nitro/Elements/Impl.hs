{-# LANGUAGE OverloadedStrings, CPP #-}
module Web.Nitro.Elements.Impl where

import Web.Nitro.Internal
import Web.Nitro.Elements
import Web.Nitro.Tags
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding as T

#include "nitro.h"

base :: Element a
base = MkBase{ELEMENT_BASE_DEFAULTS1()}

panel :: Element a
panel = base{htmlTag="div"}

list :: Element a
list = MkList{ELEMENT_BASE_DEFAULTS1(),ordered=False}

dropDown :: Element a
dropDown = MkDropDown{ELEMENT_BASE_DEFAULTS1(),options=[],value="",multiple=False,disabled=False,name=""}

radioGroup :: Element a
radioGroup = base{htmlTag="radiogroup"}

spinner :: Element a
spinner = MkSpinner{ELEMENT_BASE_DEFAULTS1(),image="/priv/static/spinner.gif"}

head :: Element a
head = base{htmlTag="head"}

metaLink :: Element a
metaLink = MkMetaLink{ELEMENT_BASE_DEFAULTS1(),href="",hreflang="",media="",rel="",sizes="",type_=""}

style_ :: Element a
style_ = MkStyle{ELEMENT_BASE_DEFAULTS1(),media="",scoped="",type_=""}

title_ :: Element a
title_ = base{htmlTag="title"}

del :: Element a
del = MkEdit{ELEMENT_BASE_DEFAULTS(),cite="",datetime="",htmlTag="del"}

ins :: Element a
ins = del{htmlTag="ins"}

area :: Element a
area = MkArea{ELEMENT_BASE_DEFAULTS1(),alt="",coords="",href="",hreflang="",media="",target="",rel="",shape="",type_=""}

audio :: Element a
audio = MkAudio{ELEMENT_BASE_DEFAULTS1(),autoplay=True,controls="",loop="",mediagroup="",muted="",preload="",src="",width=""}

canvas :: Element a
canvas = MkCanvas{ELEMENT_BASE_DEFAULTS1(),height="",width=""}

embed :: Element a
embed = MkEmbed{ELEMENT_BASE_DEFAULTS1(),height="",width="",src="",type_=""}

iframe :: Element a
iframe = MkIframe{ELEMENT_BASE_DEFAULTS1(),height="",width="",src="",srcdoc="",name="",sandbox="",seamless=""}

image_ :: Element a
image_ = MkImage{ELEMENT_BASE_DEFAULTS1(),alt="",height="",ismap="",src="",usemap=False,width="",image=""}

map_ :: Element a
map_ = MkMap{ELEMENT_BASE_DEFAULTS1(),name=""}

param :: Element a
param = MkParam{ELEMENT_BASE_DEFAULTS1(),value="",name=""}

source_ :: Element a
source_ = MkSource{ELEMENT_BASE_DEFAULTS1(),media="",src="",type_=""}

track :: Element a
track = MkTrack{ELEMENT_BASE_DEFAULTS1(),default_="",kind="",src="",srclang="",label=""}

video :: Element a
video = MkVideo{ELEMENT_BASE_DEFAULTS1(),autoplay=False,controls="",height="",loop="",mediagroup="",muted=""
               ,poster="",preload="",src="",width=""}

fieldset :: Element a
fieldset = MkFielset{ELEMENT_BASE_DEFAULTS1(),disabled=False,form="",name="",legend=""}

form_ :: Element a
form_ = MkForm{ELEMENT_BASE_DEFAULTS1(),accept_charset="",action="",autocomplete=False,enctype="",method="",name=""
             ,novalidate=False,target=""}

legend_ :: Element a
legend_ = base{htmlTag="legend"}

label_ :: Element a
label_ = MkLabel{ELEMENT_BASE_DEFAULTS1(),for="",form=""}

optgroup :: Element a
optgroup = MkOptgroup{ELEMENT_BASE_DEFAULTS1(),disabled=False,label=""}

option :: Element a
option = MkOption{ELEMENT_BASE_DEFAULTS1(),disabled=False,label="",selected=False,value=""}

output :: Element a
output = MkOutput{ELEMENT_BASE_DEFAULTS1(),for="",form="",name=""}

progress :: Element a
progress = MkProgress{ELEMENT_BASE_DEFAULTS1(),max_="",value=""}

select :: Element a
select = MkSelect{ELEMENT_BASE_DEFAULTS1(),autofocus=False,disabled=False,form="",multiple=False,name=""
                 ,required=False,size=""}

textarea :: Element a
textarea = let el = MkTextarea{ELEMENT_BASE_DEFAULTS1(),autofocus=False,cols="",dirname="",disabled=False,form=""
                     ,maxlength="",name="",placeholder="",readonly=False,required=False,rows="",wrap="",value=""}
           in el{contenteditable=True}

button:: Element a
button = MkButton{ELEMENT_BASE_DEFAULTS1(),autofocus=True,disabled=False,form="",formaction="",formmethod=""
                 ,formtarget="",formnovalidate=False,formenctype="",name="",type_="button",value=""}

literal :: Element a
literal = MkLiter {ELEMENT_BASE_DEFAULTS1(), htmlEncode = True, text = ""}

-- | Render element
render :: Element a -> BL.ByteString
render el@MkTextarea{} =
  let content_ = mconcat $ fmap (renderer el) (body el) in
  emitTag "textarea" content_ ([ ("id",    id_ el),      ("class", BS.intercalate " " (class_ el))
                               , ("style", style el),    ("title", T.encodeUtf8 $ title el)
                               , ("lang",  lang el),     ("tabindex", tabindex el)
                               , ("contenteditable", if contenteditable el then "true" else "false")
                               -- spec
                               , ("autofocus", if autofocus el then "autofocus" else "")
                               , ("cols", cols el), ("dirname", dirname el)
                               , ("disabled", if disabled el then "disabled" else "")
                               , ("form", form el), ("maxlength", maxlength el)
                               , ("name", name el), ("placeholder", placeholder el)
                               , ("readonly", if readonly el then "readonly" else "")
                               , ("required", if readonly el then "required" else "")
                               , ("rows", rows el), ("wrap", wrap el)
                               ] ++ dataFields el)
render MkLiter {htmlEncode = htmlEncode, text = text} =
  TL.encodeUtf8 $
  if htmlEncode
    then htmlEscape text
    else text
render el@MkButton{} =
  let content_ = mconcat $ fmap (renderer el) (body el) in
  emitTag "button" content_ ([("id",      id_ el),       ("class",    BS.intercalate " " (class_ el))
                            ,("style",    style el),     ("name",     name el)
                            ,("onchange", onchange el),  ("type",     type_ el)
                            ,("onclick",  onclick el),   ("disabled", if disabled el then "disabled" else "")
                            ,("value",    value el)
                            ] ++ dataFields el)
render el =
  let content_ = mconcat $ fmap (renderer el) (body el) in
  emitTag (htmlTag el) content_ ([("id",    id_ el),   ("class",    BS.intercalate " " (class_ el))
                                 ,("style", style el), ("title",    T.encodeUtf8 $ title el)
                                 ,("lang",  lang el),  ("tabindex", C8.pack $ show $ tabindex el)
                                 ,("role",  role el),  ("contenteditable", if contenteditable el then "true" else "false")
                                 ] ++ dataFields el)
