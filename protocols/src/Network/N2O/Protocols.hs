{-|
Module      : Network.N2O.Protocols
Description : N2O Protocols Starter
Copyright   : (c) Marat Khafizov, 2018
License     : BSD-3
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

N2O Protocol definitions and implementations, compatible with 
the [Erlang version of the N2O](https://github.com/synrc/n2o)

For more infomation please check out the [manual](https://haskell.n2o.space/man/protocols.htm)

-}
module Network.N2O.Protocols
 ( module Proto
 , module Network.N2O.Protocols.Nitro
-- , module Network.N2O.Protocols.Client
 , createCx) where

import Network.N2O.Types as Types
import Network.N2O.Protocols.Types as Proto
import Network.N2O.Core
import Network.N2O.Nitro
import Network.N2O.Protocols.Nitro

-- | Create context with specified @router@ middleware
createCx router = mkCx
  { cxMiddleware = [router]
  , cxProtos = [nitroProto]
  , cxDePickle = defDePickle
  , cxPickle = defPickle
  }

