{-|
Module      : Network.N2O.Web
Description : Static HTTP Server and Bridge for WebSockets
Copyright   : (c) Marat Khafizov, 2018
License     : BSD-3
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

This package provides a simple static HTTP server and adapter
for WebSockets to the N2O Protocol Loop.

Disclaimer: an HTTP server is not for production use. Please consider
to use more robust static server like NGinx or something like

* https://hackage.haskell.org/package/wai-websockets
* https://hackage.haskell.org/package/websockets-snap

-}
module Network.N2O.Web
 ( module Network.N2O.Web.WebSockets
 , module Network.N2O.Web.Http
 ) where

import Network.N2O.Web.WebSockets
import Network.N2O.Web.Http
