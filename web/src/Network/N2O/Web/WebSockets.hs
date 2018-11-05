{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.N2O.Web.WebSockets
  ( wsApp
  , mkPending
  ) where

import Control.Exception (catch, finally)
import Control.Monad (forM_, forever, mapM_)
import Data.BERT
import qualified Data.Binary as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.CaseInsensitive (mk)
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as MW
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Network.N2O.Core
import Network.N2O.Protocols.Types as Proto
import Network.N2O.Types
import Network.Socket (Socket)
import Web.Nitro
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WSConn
import qualified Network.WebSockets.Stream as WSStream
import qualified Data.Vault.Lazy as V

wsApp :: Context N2OProto a N2O -> WS.ServerApp
wsApp cx pending = do
  let path = WS.requestPath $ WS.pendingRequest pending
      cx1 = cx {cxReq = mkReq {reqPath = path}}
      handlers = cxMiddleware cx1
      applyHandlers hs ctx =
        case hs of
          [] -> ctx
          (h:hs') -> applyHandlers hs' (h ctx)
      cx2 = applyHandlers handlers cx1
      vault = V.insert contextKey cx2 V.empty
      vault' = V.insert dictKey M.empty vault
      vault'' = V.insert actionsKey [] vault'
  conn <- WS.acceptRequest pending
  ref <- newIORef vault''
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

listen :: WS.Connection -> IORef V.Vault -> IO ()
listen conn ref =
  do pid <- receiveN2O conn ref
     vault <- readIORef ref
     let cx@Context {cxProtos = protos} = fromJust $ V.lookup contextKey vault
     forever $ do
       message <- WS.receiveDataMessage conn
       case message of
         WS.Text "PING" _ -> WS.sendTextData conn ("PONG" :: T.Text)
         WS.Binary bin ->
           case B.decodeOrFail bin of
             Right (_, _, term) ->
               case fromBert term of
                 Just msg -> do
                   reply <- runN2O (protoRun msg protos) ref
                   process conn reply
                 _ -> return ()
             _ -> return ()
         _ -> error "Unknown message"
     `finally` do
    vault <- readIORef ref
    let cx@Context {cxProtos = protos} = fromJust $ V.lookup contextKey vault
    runN2O (protoRun (N2ONitro Done) protos) ref
    return ()

process conn reply =
  case reply of
    Reply a -> WS.sendBinaryData conn (B.encode $ toBert a)
    _ -> error "Unknown response type"

receiveN2O conn ref = do
  message <- WS.receiveDataMessage conn
  vault <- readIORef ref
  let cx@Context {cxProtos = protos} = fromJust $ V.lookup contextKey vault
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text bs _ ->
      case LC8.stripPrefix "N2O," bs of
        Just pid -> do
          reply <- runN2O (protoRun (N2ONitro $ Proto.Init pid) protos) ref
          process conn reply
          return pid
        _ -> error "Protocol violation"

-- | Convert Binary Erlang Terms (BERT) to the 'N2OProto' specification
fromBert :: Term -> Maybe (N2OProto a)
fromBert (TupleTerm [AtomTerm "init", BytelistTerm pid]) =
  Just $ N2ONitro (Proto.Init pid)
fromBert (TupleTerm [AtomTerm "pickle", BinaryTerm source, BinaryTerm pickled, ListTerm linked]) =
  Just $ N2ONitro (Pickle source pickled (convert linked))
  where
    convert [] = M.empty
    convert (TupleTerm [AtomTerm k, BytelistTerm v]:vs) =
      M.insert (C8.pack k) v (convert vs)
fromBert _ = Nothing

toBert :: N2OProto a -> Term
toBert (Io eval dat) =
  TupleTerm [AtomTerm "io", BytelistTerm eval, BytelistTerm dat]
