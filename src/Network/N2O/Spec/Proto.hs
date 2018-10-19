{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , TypeFamilies
            #-}

module Network.N2O.Spec.Proto where

import Data.BERT
import Control.Monad

-- | N2O protocol message
class N2OProtoMsg a where
    toBert   :: a -> Term
    fromBert :: Term -> Maybe a

-- | Each N2O protocol has an associated message type
class N2OProtoMsg (Msg a) => N2OProto a where
    type Msg a :: *
    -- canHandle :: a -> Term -> Bool
    -- canHandle _ term = case (fromBert term) :: Maybe (Msg a) of
    --                      Nothing -> False
    --                      Just _ -> True
    -- | Scetch for the info method
    info :: a -> Msg a -> IO ()

-- | N2O client message: {client,Data}, {server,Data}
data N2OClientMsg = N2OClient Term | N2OServer Term deriving Show

-- | N2OProtoMsg instance for N2O client message
instance N2OProtoMsg N2OClientMsg where
    toBert (N2OClient dat) = TupleTerm [AtomTerm "client", dat]
    toBert (N2OServer dat) = TupleTerm [AtomTerm "server", dat]
    fromBert (TupleTerm [AtomTerm "client", dat]) = Just $ N2OClient dat
    fromBert (TupleTerm [AtomTerm "server", dat]) = Just $ N2OServer dat
    fromBert _ = Nothing

-- | N2O client protocol
data N2OClientProto = N2OClientProto

-- | N2OProto instance for the N2O client protocol
instance N2OProto N2OClientProto where
    type Msg N2OClientProto = N2OClientMsg

    -- | Just an example.
    info _ msg = putStrLn $ "n2o client proto: " ++ (show $ toBert msg)

-- | N2O io protocol message
data N2OIoMsg = N2OIo Term Term | N2OBin Term deriving Show

-- | N2OProtoMsg for N2O io message
instance N2OProtoMsg N2OIoMsg where
    toBert (N2OIo eval dat) = TupleTerm [AtomTerm "io", eval, dat]
    toBert (N2OBin dat) = TupleTerm [AtomTerm "bin", dat]
    fromBert (TupleTerm [AtomTerm "io", eval, dat]) = Just $ N2OIo eval dat
    fromBert (TupleTerm [AtomTerm "bin", dat]) = Just $ N2OBin dat
    fromBert _ = Nothing

-- | N2O io protocol
data N2OIoProto = N2OIoProto

-- | N2OProto instance for n2o io protocol
instance N2OProto N2OIoProto where
    type Msg N2OIoProto = N2OIoMsg
    info _ msg = putStrLn $ "n2o io message: " ++ (show $ toBert msg)

--- Testing it:

-- | Existential for the n2o protocol
data ProtoBox = forall p. (N2OProto p) => ProtoBox p

protos :: [ProtoBox]
protos = [ProtoBox N2OClientProto, ProtoBox N2OIoProto]

msgs :: [Term]
msgs = [ TupleTerm [AtomTerm "client", AtomTerm "hello"]
       , TupleTerm [AtomTerm "io", NilTerm, NilTerm]
       ]

test :: Term -> IO ()
test m = forM_ protos (\(ProtoBox proto) ->
    case (fromBert m) of
        Nothing -> return ()
        Just x -> info proto x)

run :: IO ()
run = sequence_ [test m | m <- msgs]

-- > run
-- n2o client proto: TupleTerm [AtomTerm "client",AtomTerm "hello"]
-- n2o io message: TupleTerm [AtomTerm "io",NilTerm,NilTerm]
