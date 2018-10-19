{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
            #-}

module Network.N2O.Proto.Internal where

import Data.BERT

-- | N2O protocol message
class N2OProtoMsg a where
    toBert   :: a -> Term
    fromBert :: Term -> Maybe a

-- | Each N2O protocol has an associated message type
class N2OProtoMsg (Msg a) => N2OProto a where
    type Msg a :: *
    -- | Scetch for the info method
    info :: a -> Msg a -> IO ()

-- | N2O client message: {client,Data}, {server,Data}
data N2OClientMsg = N2OClient Term | N2OServer Term deriving Show
-- | N2O io protocol message
data N2OIoMsg = N2OIo Term Term | N2OBin Term deriving Show

-- | Existential for the n2o protocol
data ProtoBox = forall p. (N2OProto p) => ProtoBox p
