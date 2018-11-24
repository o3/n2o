#ifndef NITRO_H

#define NITRO_H

#define ELEMENT_BASE()\
     id_         :: BS.ByteString\
   , validation  :: BS.ByteString,             validate    :: BS.ByteString\
   , class_      :: [BS.ByteString],           style       :: BS.ByteString\
   , source      :: [BS.ByteString],           onclick     :: BS.ByteString\
   , onmouseover :: BS.ByteString,             onkeypress  :: BS.ByteString\
   , onblur      :: BS.ByteString,             onchange    :: BS.ByteString\
   , onkeyup     :: BS.ByteString,             onkeydown   :: BS.ByteString\
   , onfocus     :: BS.ByteString,             dataFields  :: [(BS.ByteString, BS.ByteString)]\
   , body        :: [Element a],               role        :: BS.ByteString\
   , tabindex    :: BS.ByteString,             htmlTag     :: BS.ByteString\
   , title       :: T.Text,                    postback    :: Maybe a\
   , lang        :: BS.ByteString,             contenteditable :: Bool

#define ELEMENT_BASE_DEFAULTS()\
   id_="",class_=[],style="",postback=Nothing,body=[],dataFields=[]\
  ,onfocus="",onblur="",onchange="",onclick="",onkeydown="",onkeyup="",onkeypress="",onmouseover=""\
  ,tabindex="",validation="",validate="",source=[],role="",title="",lang="",contenteditable=False

#define ELEMENT_BASE_DEFAULTS1() ELEMENT_BASE_DEFAULTS(),htmlTag=""

#define CONTROL_BASE()\
  ELEMENT_BASE()

#endif
