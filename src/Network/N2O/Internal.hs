{-# LANGUAGE ExistentialQuantification #-}
module Network.N2O.Internal where

import Data.BERT
import Control.Monad.IO.Class

data N2OCx = N2OCx
  { cxHandler :: HandlerBox -- erlang version uses first class modules for this
  }
data N2OReq = N2OReq

type N2OResp = (Term, Term, N2OReq, N2OCx)

-- | Event handler
class N2OHandler a where
  event :: (MonadIO m) => a -> Term -> m Term
  -- to be continued...

data HandlerBox = forall h. (N2OHandler h) => HandlerBox h

-- | N2O protocol
class N2OProto a where
  -- info : Proto -> Message -> Request -> State -> IO Response
  info :: (MonadIO m) => a -> Term -> N2OReq -> N2OCx -> m N2OResp

unknown, reply, binary :: Term
unknown = AtomTerm "unknown"
reply = AtomTerm "reply"
binary = AtomTerm "binary"

-- | Existential for the n2o protocol
data ProtoBox = forall p. (N2OProto p) => ProtoBox p

runProto :: (MonadIO m) => Term -> N2OReq -> N2OCx -> [ProtoBox] -> m N2OResp
runProto = go []
  where
    nop req state = (reply, TupleTerm [binary, NilTerm], req, state)
    go :: (MonadIO m) => [N2OResp] -> Term -> N2OReq -> N2OCx -> [ProtoBox] -> m N2OResp
    go _ _ req state [] = return $ nop req state
    go acc msg req state (ProtoBox proto:protos) = do
        res <- info proto msg req state
        case res of
            (AtomTerm "unknown", _, _, _) -> go acc msg req state protos
            (AtomTerm "reply", msg1, req1, state1) -> return $ (reply, msg1, req1, state1)
            a -> go (a:acc) msg req state protos
