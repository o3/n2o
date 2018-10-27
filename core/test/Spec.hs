{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Network.N2O.Internal
import Data.BERT

main :: IO ()
main = do
  (t1, _) <- protoRun (MsgTxt "") cx
  hspec $ do
    describe "nop reply test" $ do
      it "test1" $ do
        t1 `shouldBe` (Reply (MsgBin ""))
  let proto1 = MsgTxt "proto1"
  (t1, _) <- protoRun proto1 cx
  hspec $ do
    describe "proto1 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` (Reply proto1)
  let proto2 = MsgTxt "proto2"
  (t1, _) <- protoRun proto2 cx
  hspec $ do
    describe "proto2 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` (Reply proto2)

cx = mkCx{cxProtos = protos}

proto1 = Proto
  { protoInfo = \msg state ->
      case msg of
        MsgTxt "proto1" -> return (Reply (MsgTxt "proto1"), state)
        _ -> return (Unknown, state)
  , protoInit = return ()
  }
proto2 = Proto
  { protoInfo = \msg state ->
     case msg of
       MsgTxt "proto2" -> return (Reply (MsgTxt "proto2"), state)
       _ -> return (Unknown, state)
  , protoInit = return ()
  }

protos = [proto1, proto2]
