{-# LANGUAGE TypeFamilies #-}
module Network.N2O.Proto.Io where

import Network.N2O.Proto.Internal
import Network.N2O.Format

-- | N2O io protocol
data N2OIoProto = N2OIoProto

-- | N2OProto instance for n2o io protocol
instance N2OProto N2OIoProto where
    type Msg N2OIoProto = N2OIoMsg
    info _ msg = putStrLn $ "n2o io message: " ++ (show $ toBert msg)
