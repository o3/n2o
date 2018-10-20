{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Network.N2O
import Data.BERT
import Prelude (undefined, print, return)

main = runServer
  "localhost"
  3000
  defaultCx{cxProtos = [proto1], cxHandlers = [router] }

router :: N2OCx -> N2OCx
router cx@N2OCx{..} = cx{cxEvHnd = index}

index :: N2OEvHnd
index = N2OEvHnd
  { event = \ev -> do {print ev; return NilTerm} -- Just print an event
  }

proto1 :: N2OProto
proto1 = N2OProto
  { protoInit = return ()
  , protoInfo = \term _ cx@N2OCx{..} -> do {rep <- (event cxEvHnd) term; return (reply, rep, cxReq, cx)} -- Just delegate to the event handler
  }
