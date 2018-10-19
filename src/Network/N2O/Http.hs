module Network.N2O.Spec.Http
 ( HttpReq(..)
 ) where

import Network.HTTP.Types (Method, HttpVersion, RequestHeaders)

data HttpReq = HttpReq
  { httpReqMethod  :: Method
  , httpReqVersion :: HttpVersion
  , httpReqHeaders :: RequestHeaders
  -- moar fields
  }
