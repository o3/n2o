{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Network.N2O
import Data.BERT
import Prelude (undefined, print, return)

main = runServer
  "localhost"
  3000
  defaultCx{cxProtos = [proto1], cxHandlers = [router] }

router :: Cx -> Cx
router cx@Cx{..} = cx{cxEvHnd = index}

index :: EvHnd
index = EvHnd
  { event = \ev -> do {print ev; return NilTerm} -- Just print an event
  }

proto1 :: Proto
proto1 = Proto
  { protoInit = return ()
  -- pass message to the event handler as is and replay with result
  , protoInfo = \term cx@Cx{..} -> do {rep <- (event cxEvHnd) term; return (reply, rep, cx)}
  }
