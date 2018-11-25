{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.N2O.Web.WebSockets
  ( wsApp
  , mkPending
  ) where

import Control.Exception (catch, finally)
import Control.Monad (forM_, forever, mapM_)
import Data.BERT
import qualified Data.Serialize as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive (mk)
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Data.Text as T
import Data.Text.Encoding as T
import Network.N2O.Internal
import Network.Socket (Socket)
import Web.Nitro
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSConn
import qualified Network.WebSockets.Stream as WSStream

wsApp :: Context N2OProto a (StateRef a) -> WS.ServerApp
wsApp cx pending = do
  let path = WS.requestPath $ WS.pendingRequest pending
      cx1 = cx {cxReq = mkReq {reqPath = path}}
      handlers = cxMiddleware cx1
      applyHandlers hs ctx =
        case hs of
          [] -> ctx
          (h:hs') -> applyHandlers hs' (h ctx)
      cx2 = applyHandlers handlers cx1
  conn <- WS.acceptRequest pending
  ref <- newIORef $ MkState [] cx2 M.empty
  WS.forkPingThread conn 30
  listen conn ref

-- | Make pending WS request from N2O request
mkPending :: WS.ConnectionOptions -> Socket -> Req -> IO WS.PendingConnection
mkPending opts sock req = do
  stream <- WSStream.makeSocketStream sock
  let requestHead =
        WS.RequestHead
          { WS.requestPath = reqPath req
          , WS.requestSecure = False
          , WS.requestHeaders = fmap (\(k, v) -> (mk k, v)) (reqHead req)
          }
  return
    WSConn.PendingConnection
      { WSConn.pendingOptions = opts
      , WSConn.pendingRequest = requestHead
      , WSConn.pendingOnAccept = \_ -> return ()
      , WSConn.pendingStream = stream
      }

listen :: WS.Connection -> IORef (State a) -> IO ()
listen conn ref =
  do pid <- receiveN2O conn ref
     st <- readIORef ref
     let cx@Context {cxProtos = protos} = stContext st
     forever $ do
       message <- WS.receiveDataMessage conn
       case message of
         WS.Text "PING" _ -> WS.sendTextData conn ("PONG" :: T.Text)
         WS.Binary bin ->
           case B.decode (BL.toStrict bin) of
             Right term ->
               case fromBert term of
                 Just msg -> do
                   reply <- runReaderT (protoRun msg protos) ref
                   process conn reply
                 _ -> return ()
             _ -> return ()
         _ -> error "Unknown message"
     `finally` do
    st <- readIORef ref
    let cx@Context {cxProtos = protos} = stContext st
    runReaderT (protoRun (N2ONitro NitroDone) protos) ref
    return ()

process conn reply =
  case reply of
    Reply a -> WS.sendBinaryData conn (B.encode $ toBert a)
    _ -> error "Unknown response type"

receiveN2O conn ref = do
  message <- WS.receiveDataMessage conn
  st <- readIORef ref
  let cx@Context {cxProtos = protos} = stContext st
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text bs _ ->
      case C8.stripPrefix "N2O," (BL.toStrict bs) of
        Just pid -> do
          reply <- runReaderT (protoRun (N2ONitro $ NitroInit pid) protos) ref
          process conn reply
          return pid
        _ -> error "Protocol violation"

-- | Convert Binary Erlang Terms (BERT) to the 'N2OProto' specification
fromBert :: Term -> Maybe (N2OProto a)
fromBert (TupleTerm [AtomTerm "init", BytelistTerm pid]) =
  Just $ N2ONitro (NitroInit pid)
fromBert (TupleTerm [AtomTerm "pickle", BinaryTerm source, BinaryTerm pickled, ListTerm linked]) =
  Just $ N2ONitro (NitroPickle source pickled (convert linked))
  where
    convert [] = M.empty
    convert (TupleTerm [AtomTerm k, BytelistTerm v]:vs) =
      M.insert (C8.pack k) v (convert vs)
fromBert _ = Nothing

toBert :: N2OProto a -> Term
toBert (Io eval dat) =
  TupleTerm [AtomTerm "io", BytelistTerm eval, BytelistTerm dat]
