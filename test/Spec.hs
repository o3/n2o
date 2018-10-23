
import Test.Hspec
import Network.N2O.Internal
import Data.BERT

main :: IO ()
main = do
  (t1, t2, _) <- protoRun [] cx
  hspec $ do
    describe "nop reply test" $ do
      it "test1" $ do
        t1 `shouldBe` "reply"
      it "test2" $ do
        t2 `shouldBe` ["binary", []]
  let proto1 = "proto1"
  (t1, t2, _) <- protoRun proto1 cx
  hspec $ do
    describe "proto1 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` "reply"
      it "test2" $ do
        t2 `shouldBe` proto1
  let proto2 = "proto2"
  (t1, t2, _) <- protoRun proto2 cx
  hspec $ do
    describe "proto2 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` "reply"
      it "test2" $ do
        t2 `shouldBe` proto2

cx = defaultCx{cxProtos = protos}

proto1 = Proto
  { protoInfo = \msg state ->
      case msg of
        "proto1" -> return ("reply", "proto1", state)
        _ -> return ("unknown", msg, state)
  , protoInit = return ()
  }
proto2 = Proto
  { protoInfo = \msg state ->
     case msg of
       "proto2" -> return ("reply", "proto2", state)
       _ -> return ("unknown", msg, state)
  , protoInit = return ()
  }

protos = [proto1, proto2]
