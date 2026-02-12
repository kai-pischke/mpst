module WellFormedSpec (spec) where

import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Syntax
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "Well-Formedness" $ do
    it "global checker accepts simple valid protocol" $
      validateGlobalType (GMessage p q (singletonGlobal "ok" GEnd)) `shouldBe` Right ()
    it "local checker accepts simple valid protocol" $
      validateLocalType (LSend q (singletonLocal "ok" LEnd)) `shouldBe` Right ()
    it "global checker detects free type variables" $
      validateGlobalType (GVar tv) `shouldBe` Left [FreeTypeVar tv]
    it "global checker detects unguarded recursion variables" $
      validateGlobalType (GRec tv (GVar tv)) `shouldBe` Left [UnguardedTypeVar tv]
    it "global checker detects less obvious unguarded recursion" $
      validateGlobalType (GMessage p q . singletonGlobal "lab" . GRec tv . GRec tv1 . GRec tv2 . GRec tv3 $ GVar tv1)
        `shouldBe` Left [UnguardedTypeVar tv1]
    it "global checker detects self-communication" $
      validateGlobalType (GMessage p p (singletonGlobal "l" GEnd))
        `shouldBe` Left [SelfCommunication p]
    it "global checker detects duplicate labels" $
      validateGlobalType (GMessage p q (globalBranches [("dup", GEnd), ("dup", GEnd)]))
        `shouldBe` Left [DuplicateLabel (Label "dup")]
    it "global checker accepts guarded recursion" $
      validateGlobalType (GRec tv (GMessage p q (singletonGlobal "loop" (GVar tv))))
        `shouldBe` Right ()
    it "global checker accepts less obvious guarded recursion" $
      validateGlobalType (GMessage p q . singletonGlobal "lab" . GRec tv . GRec tv1 . GMessage p q . singletonGlobal "again" . GRec tv2 $ GVar tv)
        `shouldBe` Right ()
    it "local checker detects free type variables" $
      validateLocalType (LVar tv) `shouldBe` Left [FreeTypeVar tv]
    it "local checker detects unguarded recursion variables" $
      validateLocalType (LRec tv (LVar tv)) `shouldBe` Left [UnguardedTypeVar tv]
    it "local checker detects less obvious unguarded recursion variables" $
      validateLocalType (LRecv p . singletonLocal "lab" . LRec tv . LRec tv1 . LRec tv2 . LRec tv3 $ LVar tv1)
        `shouldBe` Left [UnguardedTypeVar tv1]
    it "local checker detects duplicate labels" $
      validateLocalType (LSend q (localBranches [("dup", LEnd), ("dup", LEnd)]))
        `shouldBe` Left [DuplicateLabel (Label "dup")]
    it "local checker accepts guarded recursion" $
      validateLocalType (LRec tv (LRecv q (singletonLocal "loop" (LVar tv))))
        `shouldBe` Right ()
    it "local checker accepts less obvious guarded recursion" $
      validateLocalType (LRecv p . singletonLocal "lab" . LRec tv . LRecv q $ singletonLocal "again" (LRec tv2 (LVar tv)))
        `shouldBe` Right ()
    it "checked global parser reports well-formedness errors" $
      parseGlobalTypeChecked "p -> p {l: end}" `shouldSatisfy` hasGlobalWFError
    it "checked local parser reports well-formedness errors" $
      parseLocalTypeChecked "q ! {dup: end, dup: end}" `shouldSatisfy` hasLocalWFError

hasGlobalWFError :: Either String GlobalType -> Bool
hasGlobalWFError (Left err) = "SelfCommunication" `isInfixOf` err
hasGlobalWFError (Right _) = False

hasLocalWFError :: Either String LocalType -> Bool
hasLocalWFError (Left err) = "DuplicateLabel" `isInfixOf` err
hasLocalWFError (Right _) = False

p :: Participant
p = Participant "p"

q :: Participant
q = Participant "q"

tv, tv1, tv2, tv3 :: TypeVar
tv = TypeVar "t"
tv1 = TypeVar "t1"
tv2 = TypeVar "t2"
tv3 = TypeVar "t3"

singletonGlobal :: String -> GlobalType -> NonEmpty (Label, GlobalType)
singletonGlobal lbl g = (Label lbl, g) :| []

singletonLocal :: String -> LocalType -> NonEmpty (Label, LocalType)
singletonLocal lbl t = (Label lbl, t) :| []

globalBranches :: [(String, GlobalType)] -> NonEmpty (Label, GlobalType)
globalBranches [] = error "globalBranches requires at least one branch"
globalBranches ((lbl, g) : rest) = (Label lbl, g) :| [(Label l, x) | (l, x) <- rest]

localBranches :: [(String, LocalType)] -> NonEmpty (Label, LocalType)
localBranches [] = error "localBranches requires at least one branch"
localBranches ((lbl, t) : rest) = (Label lbl, t) :| [(Label l, x) | (l, x) <- rest]
