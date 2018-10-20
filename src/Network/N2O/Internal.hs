{-# LANGUAGE ExistentialQuantification #-}
module Network.N2O.Internal where

import Data.BERT
import qualified Data.ByteString as BS
import Prelude hiding (init)

data N2OCx = N2OCx
  { cxEvHnd :: N2OEvHnd -- erlang version uses first class modules for this
  , cxProtos  :: [N2OProto] -- NOTE: erlang version does not have such field,
                            -- but it seems, that it can be placed here
  , cxReq :: N2OReq
  , cxHandlers :: [N2OCx -> N2OCx]
  }
defaultCx = N2OCx { cxReq = undefined, cxEvHnd = undefined, cxHandlers = [], cxProtos = [] }
data N2OReq = N2OReq
  { reqPath :: BS.ByteString
  }

type N2OResp = (Term, Term, N2OReq, N2OCx)

-- | Event handler
data N2OEvHnd = N2OEvHnd
  { event :: Term -> IO Term
  }
  -- to be continued...

-- | N2O protocol
data N2OProto = N2OProto
  { protoInfo :: Term -> N2OReq -> N2OCx -> IO N2OResp
  , protoInit :: IO ()
  }

unknown, reply, binary, init, terminate :: Term
unknown = AtomTerm "unknown"
reply = AtomTerm "reply"
binary = AtomTerm "binary"
init = AtomTerm "init"
terminate = AtomTerm "terminate"

runProto :: Term -> N2OReq -> N2OCx -> [N2OProto] -> IO N2OResp
runProto = go []
  where
    nop req state = (reply, TupleTerm [binary, NilTerm], req, state)
    go :: [N2OResp] -> Term -> N2OReq -> N2OCx -> [N2OProto] -> IO N2OResp
    go _ _ req state [] = return $ nop req state
    go acc msg req state (proto:protos) = do
        res <- protoInfo proto msg req state
        case res of
            (AtomTerm "unknown", _, _, _) -> go acc msg req state protos
            (AtomTerm "reply", msg1, req1, state1) -> return $ (reply, msg1, req1, state1)
            a -> go (a:acc) msg req state protos
