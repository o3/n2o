{-# LANGUAGE RecordWildCards #-}

{-| Very naive pub/sub implementation -}

module PubSub where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map               as M
import qualified Data.Text              as T

type Listener = T.Text -> IO ()

class PubSub p where
  publish :: p -> String -> T.Text -> IO ()
  subscribe :: p -> String -> Listener -> IO Int
  unsubscribe :: p -> String -> Int -> IO ()

data InMemoryPubSub = InMemory { ref  :: TVar (M.Map String [(Int, Listener)])
                               , next :: TVar Int }

instance PubSub InMemoryPubSub where
  publish InMemory {..} topic msg = do
    maybeListeners <-
      atomically $ do
        m <- readTVar $ ref
        return $ M.lookup topic m
    case maybeListeners of
      Nothing        -> return ()
      Just listeners -> sequence_ $ map snd listeners <*> [msg]
  subscribe InMemory {..} topic handler =
    atomically $ do
      m <- readTVar $ ref
      idx <- readTVar $ next
      writeTVar next $! idx + 1
      let mba = M.lookup topic m
          arr =
            case mba of
              Nothing -> [(idx, handler)]
              Just a  -> (idx, handler) : a
      writeTVar ref $ M.insert topic arr m
      return idx
  unsubscribe InMemory {..} topic idx =
    atomically $ do
      m <- readTVar $ ref
      let mba = M.lookup topic m
          arr =
            case mba of
              Nothing -> []
              Just a  -> filter ((/= idx) . fst) a
      writeTVar ref $ M.insert topic arr m


test :: IO ()
test = do
  x <- newTVarIO M.empty
  y <- newTVarIO 0
  let pubsub = InMemory x y
  tid <- forkIO $ do
    i <- subscribe pubsub "test" $ (\msg -> print msg)
    -- threadDelay 1000000
    return ()
  threadDelay 2000000
  publish pubsub "test" (T.pack "HI")
  -- threadDelay 5000000
