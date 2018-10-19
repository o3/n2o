
module Network.N2O.Format where

import Network.N2O.Proto.Internal
import Data.BERT

-- | N2OProtoMsg instance for N2O client message
instance N2OProtoMsg N2OClientMsg where
    toBert (N2OClient dat) = TupleTerm [AtomTerm "client", dat]
    toBert (N2OServer dat) = TupleTerm [AtomTerm "server", dat]
    fromBert (TupleTerm [AtomTerm "client", dat]) = Just $ N2OClient dat
    fromBert (TupleTerm [AtomTerm "server", dat]) = Just $ N2OServer dat
    fromBert _ = Nothing

-- | N2OProtoMsg for N2O io message
instance N2OProtoMsg N2OIoMsg where
    toBert (N2OIo eval dat) = TupleTerm [AtomTerm "io", eval, dat]
    toBert (N2OBin dat) = TupleTerm [AtomTerm "bin", dat]
    fromBert (TupleTerm [AtomTerm "io", eval, dat]) = Just $ N2OIo eval dat
    fromBert (TupleTerm [AtomTerm "bin", dat]) = Just $ N2OBin dat
    fromBert _ = Nothing

