{-# LANGUAGE RecordWildCards
           , OverloadedStrings #-}
module Network.N2O.WebSocket
  ( runServer
  ) where

import           Control.Exception.Safe         (catch, finally)
import           Control.Monad                  (forM_, forever, mapM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (ReaderT, ask, runReaderT)
import           Data.BERT
import qualified Data.Binary                    as B
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import qualified Network.WebSockets             as WS
import           Prelude                        hiding (init)
import Network.N2O.Internal

type ClientId  = Int
type N2O = ReaderT Cx IO

runServer ::
     String -- ^ address
  -> Int    -- ^ port
  -> Cx  -- ^ n2o context
  -> IO ()
runServer addr port cx = do
  WS.runServer addr port $ wsApp $ cx

wsApp :: Cx -> WS.ServerApp
wsApp cx pending = do
  let path = WS.requestPath $ WS.pendingRequest pending
      cx1 = cx{cxReq = Req {reqPath = path}}
      handlers = cxHandlers cx1
      applyHandlers = \hs ctx ->
        case hs of
          [] -> ctx
          (h:hs') -> applyHandlers hs' (h ctx)
      cx2 = applyHandlers handlers cx1
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  runReaderT (listen conn) cx

listen ::
     WS.Connection
  -> N2O ()
listen conn =
  do cx@Cx{..} <- ask
     pid <- liftIO $ receiveN2O conn cx
     forever $ do
       message <- liftIO $ WS.receiveDataMessage conn
       decoded <-
         case message of -- NOTE: using BERT only. TODO: encode/decode
           WS.Text t _ -> return $ BytelistTerm t
           WS.Binary bs ->
             case B.decodeOrFail bs of
               Left _ -> error "Cannot decode binary term"
               Right (_, _, term) -> return term
       reply <- liftIO $ protoRun decoded cx
       process conn reply
     `finally` do
    cx <- ask
    liftIO $ protoRun (TupleTerm [terminate, NilTerm]) cx
    return ()

process conn reply =
  case reply of
   (AtomTerm "reply", term, state) -> liftIO $ WS.sendBinaryData conn $ B.encode term
   _ -> error "Unknown response type"

receiveN2O conn cx = do
  message <- WS.receiveDataMessage conn
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text _ (Just s) ->
      case T.stripPrefix "N2O," s of
        Just pid -> do
          reply <- protoRun (TupleTerm [init, NilTerm]) cx
          process conn reply
          return pid
        _ -> error "Protocol violation"
