{-# LANGUAGE RecordWildCards
           , OverloadedStrings #-}
module Network.N2O.WebSocket
  ( wsApp
  , mkPending
  ) where

import           Control.Exception              (catch, finally)
import           Control.Monad                  (forM_, forever, mapM_)
import           Data.BERT
import qualified Data.Binary                    as B
import           Data.CaseInsensitive           (mk)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import           Network.N2O.Internal
import           Network.Socket                 (Socket)
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Connection  as WSConn
import qualified Network.WebSockets.Stream      as WSStream
import           Prelude                        hiding (init)

type ClientId  = Int

{- | N2O endpoint to the web sockets. Can be integrated with the @websockets@ library

i.e. WS.runServer addr port $ wsApp cx
-}
wsApp :: Cx -> WS.ServerApp
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

-- | Make pending WS request
mkPending :: WS.ConnectionOptions -> Req -> IO WS.PendingConnection
mkPending opts req = do
  stream <- WSStream.makeSocketStream (reqSock req)
  let requestHead = WS.RequestHead
                    { requestPath = reqPath req
                    , requestSecure = False
                    , requestHeaders = fmap (\(k,v) -> (mk k, v)) (reqHead req)}
  return WSConn.PendingConnection
    { pendingOptions = opts
    , pendingRequest = requestHead
    , pendingOnAccept = \_ -> return ()
    , pendingStream = stream
    }

listen ::
     WS.Connection
  -> Cx
  -> IO ()
listen conn cx =
  do pid <- receiveN2O conn cx
     forever $ do
       message <- WS.receiveDataMessage conn
       decoded <-
         case message of -- NOTE: using BERT only. TODO: encode/decode
           WS.Text t _ -> return $ BytelistTerm t
           WS.Binary bs ->
             case B.decodeOrFail bs of
               Left _ -> error "Cannot decode binary term"
               Right (_, _, term) -> return term
       case decoded of
         BytelistTerm "PING" -> WS.sendTextData conn ("PONG"::T.Text)
         _ -> do reply <- protoRun decoded cx
                 process conn reply
     `finally` do
    protoRun ["terminate", []] cx
    return ()

process conn reply =
  case reply of
   ("reply", term, state) -> WS.sendBinaryData conn $ B.encode term
   _ -> error "Unknown response type"

receiveN2O conn cx = do
  message <- WS.receiveDataMessage conn
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text bs _ ->
      case T.stripPrefix "N2O," (decodeUtf8 bs) of
        Just pid -> do
          reply <- protoRun ["init", []] cx
          process conn reply
          return pid
        _ -> error "Protocol violation"
