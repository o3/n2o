{-# LANGUAGE OverloadedStrings #-}
module Network.N2O.WebSockets ( wsApp , mkPending ) where

import           Control.Exception              (catch, finally)
import           Control.Monad                  (forM_, forever, mapM_)
import           Data.BERT
import           Data.CaseInsensitive           (mk)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Lazy.Char8     as LC8
import qualified Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import           Network.N2O.Internal
import           Network.Socket                 (Socket)
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Connection  as WSConn
import qualified Network.WebSockets.Stream      as WSStream

wsApp :: Cx a b -> WS.ServerApp
wsApp cx pending = do
  let path = WS.requestPath $ WS.pendingRequest pending
      cx1 = cx{cxReq = mkReq {reqPath = path}}
      handlers = cxHandlers cx1
      applyHandlers = \hs ctx ->
        case hs of
          [] -> ctx
          (h:hs') -> applyHandlers hs' (h ctx)
      cx2 = applyHandlers handlers cx1
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  listen conn cx2

-- | Make pending WS request from N2O request
mkPending :: WS.ConnectionOptions -> Req -> IO WS.PendingConnection
mkPending opts req = do
  stream <- WSStream.makeSocketStream (reqSock req)
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

listen :: WS.Connection -> Cx a b -> IO ()
listen conn cx =
  do pid <- receiveN2O conn cx
     forever $ do
       message <- WS.receiveDataMessage conn
       msg <-
         case message of
           WS.Text t _ -> return $ MsgTxt $ decodeUtf8 t
           WS.Binary bs -> return $ MsgBin bs
       case msg of
         MsgTxt "PING" -> WS.sendTextData conn ("PONG"::T.Text)
         _ -> do reply <- protoRun msg cx
                 process conn reply
     `finally` do
    protoRun MsgTerminate cx
    return ()

process conn reply =
  case reply of
   (Reply (MsgBin msg), state) -> WS.sendBinaryData conn msg
   _ -> error "Unknown response type"

receiveN2O conn cx = do
  message <- WS.receiveDataMessage conn
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text bs _ ->
      case LC8.stripPrefix "N2O," bs of
        Just pid -> do
          reply <- protoRun (MsgInit pid) cx
          process conn reply
          return pid
        _ -> error "Protocol violation"
