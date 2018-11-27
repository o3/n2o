{-|
Module      : Network.N2O
Description : Core of the N2O Framework
Copyright   : (c) Marat Khafizov, 2018
License     : BSD-3
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

This module defines basic types and functions for the N2O Framework.

One of the trickiest part of the client-server applications is the communication
protocol between client and server. This package aims to provide scalable application
level infrastructure for protocols and services.

Logically, this package consists of two parts:

  * the 'N2O' monad for local state management;
  * the 'protoRun' function, that allows to perform abstract protocol loop.

For basic usage see [N2O sample app](https://github.com/xafizoff/n2o/tree/master/samples)

-}
module Network.N2O
 ( module Network.N2O.Internal
 , module Data.BERT
 , liftIO
 , module M
 ) where

import Network.N2O.Internal
import Data.BERT
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader as M
