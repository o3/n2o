{-# LANGUAGE OverloadedStrings, CPP #-}
module Web.Nitro.Elements where

import Web.Nitro.Types (Element(..))
import Web.Nitro.Tags

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

canvas :: Element a
canvas = MkCanvas{ELEMENT_BASE_DEFAULTS1(),height="",width=""}

iframe :: Element a
iframe = MkIframe{ELEMENT_BASE_DEFAULTS1(),height="",width="",src="",srcdoc="",name="",sandbox="",seamless=""}

image_ :: Element a
image_ = MkImage{ELEMENT_BASE_DEFAULTS1(),alt="",height="",ismap="",src="",usemap=False,width="",image=""}

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

button :: Element a
button = MkButton{ELEMENT_BASE_DEFAULTS1(),autofocus=True,disabled=False,form="",formaction="",formmethod=""
                 ,formtarget="",formnovalidate=False,formenctype="",name="",type_="button",value=""}

literal :: Element a
literal = MkLiter {ELEMENT_BASE_DEFAULTS1(), htmlEncode = True, text = ""}

tr :: Element a
tr = MkTr { ELEMENT_BASE_DEFAULTS1(), cells = "" }
