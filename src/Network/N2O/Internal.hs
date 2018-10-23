{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Network.N2O.Internal where

import Data.BERT
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import Data.String (IsString, fromString)
import GHC.Exts (IsList, Item, fromList, toList)

instance IsString Term where
  fromString atom = AtomTerm atom

instance IsList Term where
  type Item Term = Term
  fromList [] = NilTerm
  fromList terms = TupleTerm terms
  toList (TupleTerm terms) = terms
  toList (ListTerm terms) = terms
  toList NilTerm = []

data Cx = Cx
  { cxEvHnd :: Term -> IO Term -- ^ erlang version uses first class modules for this
  , cxProtos  :: [Proto]       -- ^ NOTE: erlang version does not have such field,
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

mkReq = Req { reqPath = "/", reqMeth = "GET", reqVers = "HTTP/1.1", reqHead = [], reqSock = undefined }

data Resp = Resp
  { respCode :: Int
  , respHead :: [Header]
  , respBody :: BS.ByteString
  } deriving (Show)

mkResp = Resp { respCode = 200, respHead = [], respBody = BS.empty }

type Reply = (Term, Term, Cx)

-- | N2O protocol
data Proto = Proto
  { protoInfo :: Term -> Cx -> IO Reply
  , protoInit :: IO ()
  }

protoRun :: Term -> Cx -> IO Reply
protoRun msg cx = go [] msg cx (cxProtos cx)
  where
    nop :: Cx -> Reply
    nop state = ("reply", ["binary", []], state)
    go :: [Reply] -> Term -> Cx  -> [Proto] -> IO Reply
    go _ _ state [] = return $ nop state
    go acc msg state (proto:protos) = do
        res <- protoInfo proto msg state
        case res of
            ("unknown", _, _) -> go acc msg state protos
            ("reply", msg1, state1) -> return $ ("reply", msg1, state1)
            a -> go (a:acc) msg state protos
