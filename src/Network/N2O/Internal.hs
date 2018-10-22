{-# LANGUAGE ExistentialQuantification, RecordWildCards #-}
module Network.N2O.Internal where

import Data.BERT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Prelude hiding (init)
import Network.Socket (Socket)

data Cx = Cx
  { cxEvHnd :: EvHnd -- erlang version uses first class modules for this
  , cxProtos  :: [Proto] -- NOTE: erlang version does not have such field,
                            -- but it seems, that it can be placed here
  , cxReq :: Req
  , cxHandlers :: [Cx -> Cx]
  }
defaultCx = Cx { cxReq = undefined, cxEvHnd = undefined, cxHandlers = [], cxProtos = [] }
type Header = (BS.ByteString, BS.ByteString)
data Req = Req
  { reqPath :: BS.ByteString
  , reqMeth :: BS.ByteString
  , reqVers :: BS.ByteString
  , reqHead :: [Header]
  , reqSock :: Socket
  }

mkReq = Req { reqPath = C8.pack "/", reqMeth = C8.pack "GET", reqVers = C8.pack "HTTP/1.1", reqHead = [], reqSock = undefined }

data Resp = Resp
  { respCode :: Int
  , respHead :: [Header]
  , respBody :: BS.ByteString
  } deriving (Show)

mkResp = Resp { respCode = 200, respHead = [], respBody = BS.empty }

type Reply = (Term, Term, Cx)

-- | Event handler
data EvHnd = EvHnd
  { event :: Term -> IO Term
  }
  -- to be continued...

-- | N2O protocol
data Proto = Proto
  { protoInfo :: Term -> Cx -> IO Reply
  , protoInit :: IO ()
  }

unknown, reply, binary, init, terminate :: Term
unknown = AtomTerm "unknown"
reply = AtomTerm "reply"
binary = AtomTerm "binary"
init = AtomTerm "init"
terminate = AtomTerm "terminate"

protoRun :: Term -> Cx -> IO Reply
protoRun msg cx@Cx{..} = go [] msg cx cxProtos
  where
    nop state = (reply, TupleTerm [binary, NilTerm], state)
    go :: [Reply] -> Term -> Cx  -> [Proto] -> IO Reply
    go _ _ state [] = return $ nop state
    go acc msg state (proto:protos) = do
        res <- protoInfo proto msg state
        case res of
            (AtomTerm "unknown", _, _) -> go acc msg state protos
            (AtomTerm "reply", msg1, state1) -> return $ (reply, msg1, state1)
            a -> go (a:acc) msg state protos
