{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings, FlexibleContexts #-}

module Network.N2O.Web.WebSockets (wsApp, mkPending, N2OProto(N2OClient), nitroProto, mkHandler, Cx) where

import Control.Exception (catch, finally)
import Control.Monad (forM_, forever, mapM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.BERT
import qualified Data.Serialize as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
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
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM.TVar (TVar, readTVar, newTVarIO)
import Control.Concurrent.STM (atomically, modifyTVar, writeTVar)
import Control.Concurrent.STM.TChan (newBroadcastTChan, dupTChan, readTChan, writeTChan)
import Data.ByteString.Random (random)
import Text.Printf (printf)

-- | Top level sum of protocols
data N2OProto a
  = N2ONitro (Nitro a)
  | N2OClient a
  | Io BS.ByteString
       BS.ByteString
  | Nop
  --deriving (Show)

type Cx a = Context N2OProto a
type CxHandler a = Cx a -> Cx a

instance NITRO (N2O N2OProto a) where
  putActions a = do
    cx <- ask
    liftIO $ atomically $ writeTVar (cxActions cx) a
  getActions = do
    Context{cxActions = tvar} <- ask
    acts <- liftIO $ atomically $ readTVar tvar
    return acts

mkHandler h = \m -> do
  x <- h m
  return Empty

nitroProto :: (Show a, B.Serialize a) => Proto N2OProto a
nitroProto message = do
  cx@Context {cxHandler = handle,cxSessions = sess} <- getContext
  case message of
    msg@(N2ONitro (NitroInit pid)) -> do
      pid1 <- case pid of {"" -> do {rnd <- liftIO $ random(16); return $ hex rnd}; _ -> return pid}
      liftIO $ atomically $ modifyTVar sess $ \m -> M.alter (\mb -> let s = case mb of {Just s -> s; _ -> ""} in Just s) pid1 m
      handle Init
      acts <- getActions
      putActions ""
      return $ Reply $ reply acts pid1
    msg@(N2ONitro (NitroPickle _source pickled linked)) -> do
      forM_ (M.toList linked) (uncurry put)
      depickled <- depickle pickled
      case depickled of
        Just x -> do
          handle (Message x)
          acts <- getActions
          putActions ""
          return $ Reply (reply acts "")
        _ -> return Unknown
    msg@(N2OClient a) -> do
        handle $ Message a
        acts <- getActions
        putActions ""
        return $ Reply (reply acts "")
    msg@(N2ONitro NitroDone) -> do
      handle Terminate
      return Empty
  where
    reply eval dat = Io eval dat
    hex :: C8.ByteString -> C8.ByteString
    hex = C8.pack . Prelude.concatMap (printf "%02x") . C8.unpack

wsApp :: Context N2OProto a -> WS.ServerApp
wsApp cx pending = do
  tid <- myThreadId
  (chanIn,chanOut) <- atomically $ do
      chanOut <- newBroadcastTChan
      chanIn <- dupTChan chanOut
      pure (chanIn,chanOut)
  acts <- newTVarIO ""
  dict <- newTVarIO M.empty
  let path = WS.requestPath $ WS.pendingRequest pending
      cx1 = cx {cxReq = mkReq {reqPath = path}, cxInBox = chanIn, cxOutBox = chanOut, cxTid = tid, cxActions = acts, cxDict = dict}
      handlers = cxMiddleware cx1
      applyHandlers hs ctx =
        case hs of
          [] -> ctx
          (h:hs') -> applyHandlers hs' (h ctx)
      cx2 = applyHandlers handlers cx1
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
--  rChan <- atomically $ dupTChan chan
  forkIO (pump conn cx2)
  listen conn cx2

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

pump conn cx@Context{cxInBox = chan} = do
  forever $ do
    msg <- atomically $ do
        readTChan chan
--    putStrLn $ show $ cxTid cx
    reply <- runReaderT (protoRun msg $ cxProtos cx) cx
    process conn reply

listen :: WS.Connection -> Context N2OProto a -> IO ()
listen conn cx =
  do pid <- receiveN2O conn cx
     Context {cxProtos = protos, cxOutBox = chan, cxTid = tid} <- pure cx
     putStrLn $ show tid
     forever $ do
       message <- WS.receiveDataMessage conn
       case message of
         WS.Text "PING" _ -> WS.sendTextData conn ("PONG" :: T.Text)
         WS.Binary bin ->
           case B.decode (BL.toStrict bin) of
             Right term ->
               case fromBert term of
                 Just msg -> do
                   atomically $ writeTChan chan msg
                 _ -> return ()
             _ -> return ()
         _ -> error "Unknown message"
     `finally` do
    Context {cxProtos = protos} <- pure cx
    runReaderT (protoRun (N2ONitro NitroDone) protos) cx
    return ()

process conn reply =
  case reply of
    Reply a -> WS.sendBinaryData conn (B.encode $ toBert a)
    x -> error $ "Unknown response type"

receiveN2O conn cx = do
  message <- WS.receiveDataMessage conn
  Context {cxProtos = protos} <- pure cx
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text bs _ ->
      case C8.stripPrefix "N2O," (BL.toStrict bs) of
        Just pid -> do
          reply <- runReaderT (protoRun (N2ONitro $ NitroInit pid) protos) cx
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
    convert (TupleTerm [AtomTerm k, BinaryTerm v]:vs) =
      M.insert (C8.pack k) v (convert vs)
fromBert _ = Nothing

toBert :: N2OProto a -> Term
toBert (Io eval dat) =
  TupleTerm [AtomTerm "io", BinaryTerm eval, BinaryTerm dat]
