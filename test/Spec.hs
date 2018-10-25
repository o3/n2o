
import Test.Hspec
import Network.N2O.Internal
import Data.BERT

main :: IO ()
main = do
  (t1, t2, _) <- protoRun NilTerm cx
  hspec $ do
    describe "nop reply test" $ do
      it "test1" $ do
        t1 `shouldBe` (AtomTerm "reply")
      it "test2" $ do
        t2 `shouldBe` (TupleTerm [AtomTerm "binary", NilTerm])
  let proto1 = AtomTerm "proto1"
  (t1, t2, _) <- protoRun proto1 cx
  hspec $ do
    describe "proto1 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` (AtomTerm "reply")
      it "test2" $ do
        t2 `shouldBe` proto1
  let proto2 = AtomTerm "proto2"
  (t1, t2, _) <- protoRun proto2 cx
  hspec $ do
    describe "proto2 reply test" $ do
      it "test1" $ do
        t1 `shouldBe` (AtomTerm "reply")
      it "test2" $ do
        t2 `shouldBe` proto2

cx = mkCx{cxProtos = protos}

proto1 = Proto
  { protoInfo = \msg state ->
      case msg of
        AtomTerm "proto1" -> return (AtomTerm "reply", AtomTerm "proto1", state)
        _ -> return (AtomTerm "unknown", msg, state)
  , protoInit = return ()
  }
proto2 = Proto
  { protoInfo = \msg state ->
     case msg of
       AtomTerm "proto2" -> return (AtomTerm "reply", AtomTerm "proto2", state)
       _ -> return (AtomTerm "unknown", msg, state)
  , protoInit = return ()
  }

protos = [proto1, proto2]
