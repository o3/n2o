{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.N2O.Core where

import Data.BERT
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import Control.Exception (SomeException)
import Network.N2O.Types
import Data.Map.Strict (insert, (!?))

-- | @Term -> Msg@ encoder
encodeBert :: Term -> Message
encodeBert = BinaryMessage . B.encode
-- | @Term -> Msg@ decoder
decodeBert :: Message -> Maybe Term
decodeBert (BinaryMessage bin) =
  case B.decodeOrFail bin of
    Right (_,_,term) -> Just term
    _ -> Nothing
decodeBert (InitMessage pid) = Just $ TupleTerm [AtomTerm "init", BytelistTerm pid]
decodeBert TerminateMessage = Just $ AtomTerm "terminate"
decodeBert _ = Nothing

-- | 'Context' constructor
mkCx = Context
  { cxReq = undefined
  , cxHandler = undefined
  , cxMiddleware = []
  , cxDePickle = undefined
  , cxPickle = undefined
  , cxProtos = []
  , cxEncoder = undefined
  , cxDecoder = undefined
  , cxState = M.empty
  }

-- | 'Req' constructor
mkReq = Req { reqPath = "/", reqMeth = "GET", reqVers = "HTTP/1.1", reqHead = [] }

-- | NO-OP result
nop :: Result
nop = Reply (BinaryMessage BSL.empty)

-- | N2O protocol loop
protoRun :: Message -> N2O f a Result
protoRun msg = do
  ref <- ask
  cx@Context {cxProtos = protos, cxDecoder = decode} <- lift $ readIORef ref
  loop [] msg protos decode
  where
    loop _ _ [] _ = return nop
    loop acc msg (proto:protos) decoder = do
      let mbDecoded = decoder msg
      case mbDecoded of
        Just decoded -> do
          res <- protoInfo proto decoded
          case res of
            Unknown -> loop acc msg protos decoder
            Reply msg1 -> return $ Reply msg1
            a -> loop (a : acc) msg protos decoder
        _ -> loop acc msg protos decoder

-- | Lift underlying monad to the N2O monad
lift :: m a -> N2OT state m a
lift m = N2OT (const m)

-- | Get current state (env)
ask :: (Monad m) => N2OT state m state
ask = N2OT return

-- | Put data to the local state
put :: (B.Binary bin) => BS.ByteString -> bin -> N2O f a ()
put k v = do
  state <- ask
  lift $ modifyIORef state (\cx@Context{cxState=m} -> cx{cxState=insert k (B.encode v) m})

-- | Get data from the local state
get :: (B.Binary bin) => BS.ByteString -> N2O f a (Maybe bin)
get k = do
  state <- N2OT return
  cx <- lift $ readIORef state
  let mp = cxState cx
  case mp !? k of
    Just v -> return $ Just (B.decode v)
    _ -> return Nothing
