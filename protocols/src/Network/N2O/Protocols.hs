{-|
Module      : Network.N2O.Protocols
Description : N2O Protocols Starter
Copyright   : (c) Marat Khafizov, 2018
License     : BSD-3
Maintainer  : xafizoff@gmail.com
Stability   : experimental
Portability : not portable

N2O Protocol definitions and implementations, compatible with 
the (Erlang version of the N2O)[https://github.com/synrc/n2o]

For more infomation please check out the (manual)[https://haskell.n2o.space/man/protocols.htm]

-}
module Network.N2O.Protocols
 ( module Proto
 , module Network.N2O.Protocols.Nitro
 , module Network.N2O.Protocols.Client
 , readBert, createCx, defDecoder, defEncoder) where

import Network.N2O.Types as Types
import Network.N2O.Core
import Network.N2O.Nitro
import Network.N2O.Protocols.Types as Proto
import Network.N2O.Protocols.Nitro
import Network.N2O.Protocols.Client
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as C8
import Data.BERT
import Data.IORef
import qualified Data.Binary as B
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Base64.Lazy as B64
import Control.Monad (forM_)

-- | Convert Binary Erlang Terms (BERT) to the 'N2OProto' specification
readBert :: Term -> Maybe (N2OProto a)
readBert (TupleTerm [AtomTerm "init", BytelistTerm pid]) = Just $ N2ONitro (Proto.Init pid)
readBert (TupleTerm [AtomTerm "pickle", BinaryTerm source, BinaryTerm pickled, ListTerm linked]) =
    Just $ N2ONitro (Pickle source pickled (convert linked))
  where
    convert [] = M.empty
    convert (TupleTerm [AtomTerm k, BytelistTerm v] : vs) = M.insert (C8.pack k) v (convert vs)
readBert _ = Nothing

-- | Create context with specified @router@ middleware
createCx router = mkCx
  { cxMiddleware = [router]
  , cxProtos = [nitroProto]
  , cxDecoder = defDecoder
  , cxEncoder = defEncoder
  , cxDePickle = defDePickle
  , cxPickle = defPickle
  }

-- | default decoder
defDecoder msg =
  case decodeBert msg of
    Just term -> case readBert term of
                   Just x -> Just x
                   _ -> Nothing
    _ -> Nothing
-- | default encoder
defEncoder bs = BinaryMessage (B.encode (TupleTerm [AtomTerm "io", BytelistTerm bs, NilTerm]))

