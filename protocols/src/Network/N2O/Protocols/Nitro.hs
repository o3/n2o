{-# LANGUAGE ScopedTypeVariables #-}
module Network.N2O.Protocols.Nitro where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.IORef
import Data.BERT
import Network.N2O.Core
import Network.N2O.Types as Types
import Web.Nitro
import Network.N2O.Protocols.Types as Proto

nitroProto :: (Show a) => Proto N2OProto a N2O
nitroProto = Proto { protoInfo = nitroInfo }

nitroInfo :: (Show a) => N2OProto a -> N2O (Result (N2OProto a))
nitroInfo message = do
  ref <- ask
  cx@Context {cxHandler = handle, cxDePickle = dePickle} <- getContext
  lift $ putStrLn ("NITRO : " ++ show message)
  case message of
    msg@(N2ONitro (Proto.Init pid)) -> do
      handle Types.Init
      actions <- getActions
      rendered <- renderActions' actions
      return $ Reply (reply rendered)
    msg@(N2ONitro (Pickle _source pickled linked)) -> do
      forM_ (M.toList linked) (uncurry put)
      case dePickle pickled of
        Just x -> do
          handle (Message x)
          actions <- getActions
          rendered <- renderActions' actions
          return $ Reply (reply rendered)
        _ -> return Unknown
    msg@(N2ONitro Done) -> do
      handle Terminate
      return Empty
  where
    reply bs = Io bs L.empty
    renderActions' actions =
      case actions of
        [] -> return L.empty
        actions -> do
          putActions []
          first <- renderActions actions
          actions2 <- getActions
          second <- renderActions actions2
          putActions []
          return $ first <> CL8.pack ";" <> second
