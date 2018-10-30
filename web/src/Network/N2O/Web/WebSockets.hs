{-# LANGUAGE OverloadedStrings #-}
module Network.N2O.Web.WebSockets ( wsApp , mkPending ) where

import           Control.Exception              (catch, finally)
import           Control.Monad                  (forM_, forever, mapM_)
import           Data.BERT
import           Data.CaseInsensitive           (mk)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Lazy.Char8     as LC8
import qualified Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import           Network.N2O.Types
import           Network.N2O.Core
import           Network.Socket                 (Socket)
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Connection  as WSConn
import qualified Network.WebSockets.Stream      as WSStream
import           Data.IORef
import qualified Data.Map.Strict                as M

wsApp :: Context f a b -> WS.ServerApp
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
  ref <- newIORef cx2
  WS.forkPingThread conn 30
  listen conn ref

-- | Make pending WS request from N2O request
mkPending :: WS.ConnectionOptions -> Socket -> Req -> IO WS.PendingConnection
mkPending opts sock req = do
  stream <- WSStream.makeSocketStream sock
  let requestHead = WS.RequestHead
                    { WS.requestPath = reqPath req
                    , WS.requestSecure = False
                    , WS.requestHeaders = fmap (\(k,v) -> (mk k, v)) (reqHead req)}
  return WSConn.PendingConnection
    { WSConn.pendingOptions = opts
    , WSConn.pendingRequest = requestHead
    , WSConn.pendingOnAccept = \_ -> return ()
    , WSConn.pendingStream = stream
    }

listen :: WS.Connection -> State f a b -> IO ()
listen conn state =
  do pid <- receiveN2O conn state
     forever $ do
       message <- WS.receiveDataMessage conn
       msg <-
         case message of
           WS.Text t _ -> return $ TextMessage $ decodeUtf8 t
           WS.Binary bs -> return $ BinaryMessage bs
       case msg of
         TextMessage "PING" -> WS.sendTextData conn ("PONG"::T.Text)
         _ -> do reply <- runN2O (protoRun msg) state
                 process conn reply
     `finally` do
    runN2O (protoRun TerminateMessage) state
    return ()

process conn reply =
  case reply of
   Reply (BinaryMessage msg) -> WS.sendBinaryData conn msg
   _ -> error "Unknown response type"

receiveN2O conn state = do
  message <- WS.receiveDataMessage conn
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text bs _ ->
      case LC8.stripPrefix "N2O," bs of
        Just pid -> do
          reply <- runN2O (protoRun (InitMessage pid)) state
          process conn reply
          return pid
        _ -> error "Protocol violation"
