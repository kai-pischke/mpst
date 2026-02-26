module MpstkBackendSpec (spec) where

import qualified Data.Map.Strict as Map
import Syntax (LocalType(..), Participant(..), TypeVar(..), parseLocalTypeChecked)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)
import MpstkBackend (toMpstkLocalType, toMpstkCtx, MpstkResults(..), mpstkVerify)

spec :: Spec
spec =
  describe "mpstk backend" $ do
    describe "toMpstkLocalType" $ do
      it "translates end" $
        toMpstkLocalType LEnd `shouldBe` "end"

      it "translates a type variable" $
        toMpstkLocalType (LVar (TypeVar "t")) `shouldBe` "t"

      it "translates a single-branch send" $
        expectTranslation
          "q ! {ok: end}"
          "q (+) ok . end"

      it "translates a multi-branch send" $
        expectTranslation
          "q ! {l1: end, l2: end}"
          "q (+) {l1 . end, l2 . end}"

      it "translates a single-branch receive" $
        expectTranslation
          "q ? {ok: end}"
          "q & ok . end"

      it "translates a multi-branch receive" $
        expectTranslation
          "q ? {l1: end, l2: end}"
          "q & {l1 . end, l2 . end}"

      it "translates recursion" $
        expectTranslation
          "rec t . q ! {go: t}"
          "rec(t) q (+) go . t"

      it "translates nested send/receive" $
        expectTranslation
          "q ! {k: r ? {ack: end}}"
          "q (+) k . r & ack . end"

    describe "toMpstkCtx" $ do
      it "translates a two-party context" $
        expectCtxTranslation
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l2: end}")]
          "s[p]: q (+) {l1 . end, l2 . end},\ns[q]: p & {l1 . end, l2 . end}"

    describe "mpstkVerify (integration)" $ do
      it "reports safe + deadlock-free + live for matched send/receive loop" $
        expectMpstkResults
          [("p", "rec t . q ! {a: t}"), ("q", "rec t . p ? {a: t}")]
          MpstkResults { mpstkSafe = True, mpstkDeadlockFree = True, mpstkLive = True }

      it "reports unsafe for mismatched labels" $
        expectMpstkResults
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l3: end}")]
          MpstkResults { mpstkSafe = False, mpstkDeadlockFree = True, mpstkLive = False }

      it "reports safe but not deadlock-free for one-sided termination" $
        expectMpstkResults
          [("p", "q ! {l1: q ! {l2: end}}"), ("q", "p ? {l1: end}")]
          MpstkResults { mpstkSafe = True, mpstkDeadlockFree = False, mpstkLive = False }

      it "reports safe + deadlock-free + live for simple terminating protocol" $
        expectMpstkResults
          [("p", "q ! {ok: end}"), ("q", "p ? {ok: end}")]
          MpstkResults { mpstkSafe = True, mpstkDeadlockFree = True, mpstkLive = True }

expectMpstkResults :: [(String, String)] -> MpstkResults -> IO ()
expectMpstkResults pairs expected = do
  let parsed = mapM parsePair pairs
  case parsed of
    Left err -> expectationFailure ("parse error: " ++ err)
    Right entries -> do
      results <- mpstkVerify (Map.fromList entries)
      results `shouldBe` expected
  where
    parsePair (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left err
        Right lt -> Right (Participant name, lt)

expectTranslation :: String -> String -> IO ()
expectTranslation input expected =
  case parseLocalTypeChecked input of
    Left err -> expectationFailure ("parse error: " ++ err)
    Right lt -> toMpstkLocalType lt `shouldBe` expected

expectCtxTranslation :: [(String, String)] -> String -> IO ()
expectCtxTranslation pairs expected = do
  let parsed = mapM parsePair pairs
  case parsed of
    Left err -> expectationFailure ("parse error: " ++ err)
    Right entries -> toMpstkCtx (Map.fromList entries) `shouldBe` expected
  where
    parsePair (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left err
        Right lt -> Right (Participant name, lt)
