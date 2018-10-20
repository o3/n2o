{-# LANGUAGE ExistentialQuantification #-}
module Network.N2O.Internal where

import Data.BERT

data N2OCx = N2OCx
  { cxHandler :: N2OHandler -- erlang version uses first class modules for this
  }
data N2OReq = N2OReq

type N2OResp = (Term, Term, N2OReq, N2OCx)

-- | Event handler
data N2OHandler = N2OHandler
  { event :: Term -> IO Term
  }
  -- to be continued...

-- | N2O protocol
data N2OProto = N2OProto
  { info :: Term -> N2OReq -> N2OCx -> IO N2OResp
  }

unknown, reply, binary :: Term
unknown = AtomTerm "unknown"
reply = AtomTerm "reply"
binary = AtomTerm "binary"

runProto :: Term -> N2OReq -> N2OCx -> [N2OProto] -> IO N2OResp
runProto = go []
  where
    nop req state = (reply, TupleTerm [binary, NilTerm], req, state)
    go :: [N2OResp] -> Term -> N2OReq -> N2OCx -> [N2OProto] -> IO N2OResp
    go _ _ req state [] = return $ nop req state
    go acc msg req state (proto:protos) = do
        res <- info proto msg req state
        case res of
            (AtomTerm "unknown", _, _, _) -> go acc msg req state protos
            (AtomTerm "reply", msg1, req1, state1) -> return $ (reply, msg1, req1, state1)
            a -> go (a:acc) msg req state protos
