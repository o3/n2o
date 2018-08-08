{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.N2O
  ( runServer
  , defaultConfig
  , broadcast
  , allClients
  , send
  , eval
  , b2t
  , t2b
  , q
  , Handler
  , N2O
  , State
  , GlobalState(..)
  , LocalState(..)
  ) where

import           Control.Concurrent.STM
import           Control.Exception.Safe         (catch, finally)
import           Control.Monad                  (forM_, forever, mapM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (ReaderT, ask, runReaderT)
import           Data.BERT
import qualified Data.Binary                    as B
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Lazy.Char8     as C8
import qualified Data.Map                       as M
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import qualified Network.HTTP.Types             as Http
import           Network.N2O.Nitro
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import           Network.Wai.Middleware.Static  (static)
import qualified Network.WebSockets             as WS

--TODO: make it UUID or some random string
type ClientId  = Int
data GlobalState = GlobalState
  { nextId  :: TVar ClientId -- counter
  , clients :: TVar (M.Map ClientId WS.Connection) -- global state
  }
data LocalState = LocalState
  { terms    :: TVar (M.Map BL.ByteString Term) -- local terms store
  , clientId :: ClientId
  }
type State = (LocalState, GlobalState)
type N2O = ReaderT State IO
instance Semigroup a => Semigroup (N2O a) where
  a <> b = do
    x <- a
    y <- b
    return $ x <> y
instance (IsString a, Monoid a) => Monoid (N2O a) where
  mempty = return ""
  mappend = (<>)

type Handler a = [Term] -> N2O a

data Config = Config
  { app :: Wai.Application
  , port :: Warp.Port
  }

defaultConfig = Config {port = 3000, app = (static notFound)}
notFound _ respond =
  respond $
  Wai.responseLBS
    Http.status404
    [("Content-Type", "text/plain")]
    "404 - Not found"

runServer :: Config -> Handler () -> IO ()
runServer Config {..} handle = do
  state <-
    atomically $ do
      clients <- newTVar M.empty
      initialId <- newTVar 0
      return $ GlobalState initialId clients
  Warp.run port $
    WS.websocketsOr WS.defaultConnectionOptions (wsApp state handle) app

wsApp :: GlobalState -> Handler () -> WS.ServerApp
wsApp globalState handle pending = do
  conn <- WS.acceptRequest pending
  clientId <- connectClient conn globalState
  localState <-
    atomically $ do
      table <- newTVar M.empty
      return $ LocalState table clientId
  WS.forkPingThread conn 30
  runReaderT (listen conn handle) (localState, globalState)

--TODO: handle messages asynchronously
listen :: WS.Connection -> Handler () ->  N2O ()
listen conn handle =
  do sid <- liftIO $ receiveN2O conn
     (LocalState {..}, stateRef) <- ask
     liftIO $ putStrLn $ "SID : " ++ show sid
  -- reload page after reconnect
     case sid of
       NilTerm -> do
         send $ T.pack $ "transition.pid = '" ++ (show clientId) ++ "';"
       _ -> do
         send "window.top.location='';"
     handle [AtomTerm "init"]
     forever $ do
       message <- liftIO $ receive conn
       case message of
         (AtomTerm "pickle":_:(BinaryTerm pickled):(ListTerm (_:rs)):[]) -> do
           liftIO $
             atomically $ do
               t <- readTVar terms
               let t' = loop rs t
               writeTVar terms t'
           handle [(depickle pickled)]
         _ -> handle message
     `finally` do
    disconnectClient
    handle [AtomTerm "disconnect"]
  where
    loop [] t                                     = t
    loop (r@(TupleTerm [BytelistTerm k, v]):rs) t = M.insert k v $ loop rs t

q :: BL.ByteString -> N2O (Maybe Term)
q k = do
  (LocalState {..},_) <- ask
  liftIO $ atomically $ do
    t <- readTVar terms
    return $ M.lookup k t

receive conn = do
  let loop = receive conn
  message <- WS.receiveDataMessage conn
  decoded <-
    case message of
      WS.Binary x ->
        case B.decode x of
          TupleTerm x -> return x
          _           -> error "Protocol violation: expected tuple"
      WS.Text x _ -> do
        case x of "PING" -> WS.sendTextData conn ("PONG" :: T.Text)
        loop
  return decoded

receiveN2O connection = do
  message <- WS.receiveDataMessage connection
  case message of
    WS.Binary _ -> error "Protocol violation: expected text message"
    WS.Text "" _ -> error "Protocol violation: got empty text"
    WS.Text s _ ->
      case C8.stripPrefix "N2O," s of
        Just "" -> return NilTerm
        Just x -> do
          putStrLn $ show x
          return $ BinaryTerm x
        _ -> error "Protocol violation"

broadcast :: T.Text -> N2O ()
broadcast msg = do
  c <- allClients
  liftIO $ forM_ (M.toList c) (\(_, conn) -> send_ conn msg)

send_ conn = WS.sendBinaryData conn . eval

send :: Action -> N2O ()
send action = do
  (LocalState {..}, state) <- ask
  c <- allClients
  case M.lookup clientId c of
    Nothing   -> do
      liftIO $ putStrLn $ "clientId not found " ++ show clientId
      return ()
    Just conn -> liftIO $ send_ conn action

b2t :: BL.ByteString -> T.Text
b2t = decodeUtf8 . BL.toStrict

t2b :: T.Text -> BL.ByteString
t2b = BL.fromStrict . encodeUtf8

eval :: T.Text -> BL.ByteString
eval x = B.encode $ TupleTerm [AtomTerm "io", showBERT $ t2b x, NilTerm]

allClients :: N2O (M.Map ClientId WS.Connection)
allClients = do
  (_, GlobalState {..}) <- ask
  liftIO $ readTVarIO clients

connectClient :: WS.Connection -> GlobalState -> IO ClientId
connectClient conn GlobalState {..} = do
  liftIO $ atomically $ do
    clientId <- readTVar nextId
    writeTVar nextId $! clientId + 1
    oldClients <- readTVar clients
    let newClients = M.insert clientId conn oldClients
    writeTVar clients newClients
    return clientId

disconnectClient :: N2O ()
disconnectClient = do
  (LocalState {..}, GlobalState {..}) <- ask
  liftIO $ atomically $ do
    oldClients <- readTVar clients
    let newClients = M.delete clientId oldClients
    writeTVar clients newClients
    return ()
