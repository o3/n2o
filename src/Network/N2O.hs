{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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
  , wire
  , actions
  , flush
  , local
  , global
  , Handler
  , N2O
  , N2OMessage(init, destroy)
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
import           Data.List                      (intersperse)
import qualified Data.Map                       as M
import           Data.Proxy
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           GHC.Generics
import qualified Network.HTTP.Types             as Http
import           Network.N2O.Generic
import           Network.N2O.Nitro
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import           Network.Wai.Middleware.Static  (static)
import qualified Network.WebSockets             as WS
import           Prelude                        hiding (init)

class N2OMessage t where
  init :: t
  destroy :: t
  names :: Proxy t -> [String]

  default names :: (Generic t, Names' (Rep t)) => Proxy t -> [String]
  names _ = names' (Proxy :: Proxy (Rep t))

type ClientId  = Int
data GlobalState = GlobalState
  { nextId  :: TVar ClientId -- counter
  , clients :: TVar (M.Map ClientId WS.Connection) -- global state
  }
data LocalState = LocalState
  { clientId :: ClientId
  }
newtype Wire = Wire { wiredActions :: TVar [Action] }
type State = (LocalState, GlobalState, Wire)
type N2O = ReaderT State IO
instance Nitro N2O where
  wire action = do                
    Wire w <- wired               
    liftIO $ atomically $ do      
      x <- readTVar w             
      writeTVar w (x ++ [action]) 
  actions = do
    Wire w <- wired
    liftIO $ atomically $ do
      x <- readTVar w
      return x
  clear = do
    Wire w <- wired
    liftIO $ atomically $ do
      writeTVar w []

type Handler p a = p -> N2O a

data Config = Config
  { app  :: Wai.Application
  , port :: Warp.Port
  }

defaultConfig = Config {port = 3000, app = (static notFound)}
notFound _ respond =
  respond $
  Wai.responseLBS
    Http.status404
    [("Content-Type", "text/plain")]
    "404 - Not found"

runServer ::
     forall p. (B.Binary p, N2OMessage p)
  => Config
  -> Handler p ()
  -> IO ()
runServer Config {..} handle = do
  (state, wire) <-
    atomically $ do
      clients <- newTVar M.empty
      initialId <- newTVar 0
      wire <- newTVar []
      return $ (GlobalState initialId clients, Wire wire)
  Warp.run port $
    WS.websocketsOr WS.defaultConnectionOptions (wsApp (state, wire) handle) app

wsApp ::
      forall p. (B.Binary p, N2OMessage p)
   => (GlobalState, Wire)
   -> Handler p ()
   -> WS.ServerApp
wsApp (globalState, wire) handle pending = do
  conn <- WS.acceptRequest pending
  clientId <- connectClient conn globalState
  localState <-
    atomically $ do
      -- table <- newTVar M.empty
      return $ LocalState clientId
  WS.forkPingThread conn 30
  runReaderT (listen conn handle) (localState, globalState, wire)

listen ::
     forall p. (B.Binary p, N2OMessage p)
  => WS.Connection
  -> Handler p ()
  -> N2O ()
listen conn handle =
  do sid <- liftIO $ receiveN2O conn
     (LocalState {..}, stateRef, _) <- ask
     liftIO $ putStrLn $ "SID : " ++ show sid
     let names_ = names (Proxy :: Proxy p)
     liftIO $ print $ names_
  -- reload page after reconnect
     case sid of
       "" -> do
         send $ T.pack $ "transition.pid = '" ++ (show clientId) ++ "';"
       _ -> do
         send "window.top.location='';"
     let eventListAction =
           "transition.events = [" ++
           (concat $ intersperse "," $ (map (\x -> "'" ++ x ++ "'") names_)) ++
           "];"
     liftIO $ print eventListAction
     send $ T.pack $ eventListAction
     handle init
     flush
     forever $ do
       message <- liftIO $ receive conn
       handle message
       flush
     `finally` do
    disconnectClient
    handle destroy

receive :: (B.Binary p, N2OMessage p) => WS.Connection -> IO p
receive conn = do
  let loop = receive conn
  message <- WS.receiveDataMessage conn
  decoded <-
    case message of
      WS.Binary x ->
        case B.decodeOrFail x of
          Right (_, _, x) -> return x
          _               -> error "Cannot decode message"
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
        Just "" -> return ""
        Just x -> do
          putStrLn $ show x
          return x
        _ -> error "Protocol violation"

broadcast :: N2O ()
broadcast = do
  c <- allClients
  msgs <- actions
  liftIO $ forM_ msgs (\msg -> forM_ (M.toList c) (\(_, conn) -> send_ conn msg))
  clear

send_ conn = WS.sendBinaryData conn . eval

send :: Action -> N2O ()
send action = do
  LocalState {..} <- local
  c <- allClients
  case M.lookup clientId c of
    Nothing   -> do
      liftIO $ putStrLn $ "clientId not found " ++ show clientId
      return ()
    Just conn -> liftIO $ send_ conn action

global :: N2O GlobalState
global = do
  (_,g,_) <- ask
  return g

local :: N2O LocalState
local = do
  (s,_,_) <- ask
  return s

wired :: N2O Wire
wired = do
  (_,_,w) <- ask
  return w

flush :: N2O ()
flush = do
  Wire w <- wired
  xs <- liftIO $ atomically $ do
    xs <- readTVar w
    writeTVar w []
    return xs
  forM_ xs (\a -> send a)

b2t :: BL.ByteString -> T.Text
b2t = decodeUtf8 . BL.toStrict

t2b :: T.Text -> BL.ByteString
t2b = BL.fromStrict . encodeUtf8

eval :: T.Text -> BL.ByteString
eval x = B.encode $ TupleTerm [AtomTerm "io", showBERT $ t2b x, NilTerm]

allClients :: N2O (M.Map ClientId WS.Connection)
allClients = do
  GlobalState {..} <- global
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
  (LocalState {..}, GlobalState {..}, _) <- ask
  liftIO $ atomically $ do
    oldClients <- readTVar clients
    let newClients = M.delete clientId oldClients
    writeTVar clients newClients
    return ()
