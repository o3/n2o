module Network.N2O.Protocols
 ( module Network.N2O.Protocols.Types
 , module Network.N2O.Protocols.Nitro
 , module Network.N2O.Protocols.Client
 , readBert, createCx, defDecoder, defEncoder) where

import Network.N2O.Types
import Network.N2O.Core
import Network.N2O.Nitro
import Network.N2O.Protocols.Types
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

readBert (TupleTerm [AtomTerm "init", BytelistTerm pid]) = Just $ N2ONitro (I pid)
readBert (TupleTerm [AtomTerm "pickle", BinaryTerm source, BinaryTerm pickled, ListTerm linked]) =
    Just $ N2ONitro (P source pickled (convert linked))
  where
    convert [] = M.empty
    convert (TupleTerm [AtomTerm k, BytelistTerm v] : vs) = M.insert (C8.pack k) v (convert vs)
readBert _ = Nothing

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
defEncoder bs = MsgBin (B.encode (TupleTerm [AtomTerm "io", BytelistTerm bs, NilTerm]))

