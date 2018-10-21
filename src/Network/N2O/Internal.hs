{-# LANGUAGE ExistentialQuantification, RecordWildCards #-}
module Network.N2O.Internal where

import Data.BERT
import qualified Data.ByteString as BS
import Prelude hiding (init)

data Cx = Cx
  { cxEvHnd :: EvHnd -- erlang version uses first class modules for this
  , cxProtos  :: [Proto] -- NOTE: erlang version does not have such field,
                            -- but it seems, that it can be placed here
  , cxReq :: Req
  , cxHandlers :: [Cx -> Cx]
  }
defaultCx = Cx { cxReq = undefined, cxEvHnd = undefined, cxHandlers = [], cxProtos = [] }
data Req = Req
  { reqPath :: BS.ByteString
  }

type Resp = (Term, Term, Cx)

-- | Event handler
data EvHnd = EvHnd
  { event :: Term -> IO Term
  }
  -- to be continued...

-- | N2O protocol
data Proto = Proto
  { protoInfo :: Term -> Cx -> IO Resp
  , protoInit :: IO ()
  }

unknown, reply, binary, init, terminate :: Term
unknown = AtomTerm "unknown"
reply = AtomTerm "reply"
binary = AtomTerm "binary"
init = AtomTerm "init"
terminate = AtomTerm "terminate"

protoRun :: Term -> Cx -> IO Resp
protoRun msg cx@Cx{..} = go [] msg cx cxProtos
  where
    nop state = (reply, TupleTerm [binary, NilTerm], state)
    go :: [Resp] -> Term -> Cx  -> [Proto] -> IO Resp
    go _ _ state [] = return $ nop state
    go acc msg state (proto:protos) = do
        res <- protoInfo proto msg state
        case res of
            (AtomTerm "unknown", _, _) -> go acc msg state protos
            (AtomTerm "reply", msg1, state1) -> return $ (reply, msg1, state1)
            a -> go (a:acc) msg state protos
