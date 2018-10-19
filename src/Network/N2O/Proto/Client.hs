{-# LANGUAGE TypeFamilies #-}
module Network.N2O.Proto.Client where

import Data.BERT
import Network.N2O.Proto.Internal
import Network.N2O.Format

-- | N2O client protocol
data N2OClientProto = N2OClientProto

-- | N2OProto instance for the N2O client protocol
instance N2OProto N2OClientProto where
    type Msg N2OClientProto = N2OClientMsg
    -- | Just an example.
    info _ msg = putStrLn $ "n2o client proto: " ++ (show $ toBert msg)
