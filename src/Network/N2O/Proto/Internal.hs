{-# LANGUAGE ExistentialQuantification #-}

module Network.N2O.Proto.Internal where

import Data.BERT
import Control.Monad.IO.Class

-- | N2O protocol
class N2OProto a where
  -- info : Proto -> Message -> Request -> State -> IO Response
  info :: (MonadIO m) => a -> Term -> Term -> Term -> m (Term)

unknown, reply :: Term
unknown = AtomTerm "unknown"
reply = AtomTerm "reply"
binary = AtomTerm "binary"

-- | Existential for the n2o protocol
data ProtoBox = forall p. (N2OProto p) => ProtoBox p

runProto :: (MonadIO m) => Term -> Term -> Term -> [ProtoBox] -> m (Term)
runProto = go []
  where
    nop req state = TupleTerm [reply, TupleTerm [binary, NilTerm], req, state]
    go :: (MonadIO m) => [Term] -> Term -> Term -> Term -> [ProtoBox] -> m (Term)
    go _ _ req state [] = return $ nop req state
    go acc msg req state (ProtoBox proto:protos) = do
        res <- info proto msg req state
        case res of
            TupleTerm [AtomTerm "unknown", _, _] -> go acc msg req state protos
            TupleTerm [AtomTerm "reply", msg1, req1, state1] -> return $ TupleTerm [reply, msg1, req1, state1]
            a -> go (a:acc) msg req state protos
