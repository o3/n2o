
import Test.Hspec
import Network.N2O.Internal
import Data.BERT

main :: IO ()
main = do
  (t1, t2, _, _) <- runProto NilTerm undefined undefined protos
  hspec $ do
    describe "nop reply test" $ do
      it "test1" $ do
        t1 `shouldBe` reply
      it "test2" $ do
        t2 `shouldBe` (TupleTerm [binary, NilTerm])
  let proto1 = AtomTerm "proto1"
  (t1, t2, _, _) <- runProto proto1 undefined undefined protos
  hspec $ do
    describe "proto1 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` reply
      it "test2" $ do
        t2 `shouldBe` proto1
  let proto2 = AtomTerm "proto2"
  (t1, t2, _, _) <- runProto proto2 undefined undefined protos
  hspec $ do
    describe "proto2 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` reply
      it "test2" $ do
        t2 `shouldBe` proto2

proto1 = N2OProto
  { protoInfo = \msg req state ->
      case msg of
        p1@(AtomTerm "proto1") -> return (reply, p1, req, state)
        _ -> return (unknown, msg, req, state)
  , protoInit = undefined
  }
proto2 = N2OProto
  { protoInfo = \msg req state ->
     case msg of
       p2@(AtomTerm "proto2") -> return (reply, p2, req, state)
       _ -> return (unknown, msg, req, state)
  , protoInit = undefined
  }

protos = [proto1, proto2]
