module SafetySpec (spec) where

import Automata (ContextGraph, buildContextGraph, buildLocalGraph)
import Safety (SafetyError(..), checkSafety)
import Syntax (Label(..), Participant(..), parseLocalTypeChecked)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "3) Safety checking" $ do
    it "[SAFE-001] accepts a protocol with matched sends and receives" $
      expectSafe
        [ ("p", "q ! {l1: end, l2: end}")
        , ("q", "p ? {l1: end, l2: end}")
        ]
    it "[SAFE-002] rejects states where some enabled sends lack matching enabled receives" $
      expectUnsafeWithLabel (Label "l2")
        [ ("p", "q ! {l1: end, l2: end}")
        , ("q", "p ? {l1: end, l3: end}")
        ]
    it "[SAFE-003] accepts when some enabled receives lack matching enabled sends" $
      expectSafe
        [ ("p", "q ! {l1: end}")
        , ("q", "p ? {l1: end, l3: end}")
        ]
    it "[SAFE-004] accepts states where only sends or only receives are enabled for a pair" $
      expectSafe
        [ ("p", "q ! {l1: end}")
        , ("q", "p ! {l2: end}")
        ]
    it "[SAFE-005] detects safety violation under unfolding" $
      expectUnsafeWithLabel (Label "l")
        [ ("p", "rec t . q ! {l: t}")
        , ("q", "rec t . p ? {l: p ? {l1: t}}")
        ]
    it "[SAFE-006] accepts safe unfolding" $
      expectSafe
        [ ("p", "rec t . q ! {l: t}")
        , ("q", "rec t . p ? {l: p ? {l: t}}")
        ]

expectSafe :: [(String, String)] -> Expectation
expectSafe participantsAndLocals =
  case parseAsContextGraph participantsAndLocals of
    Left err -> expectationFailure err
    Right cg -> checkSafety cg `shouldBe` Right ()

expectUnsafeWithLabel :: Label -> [(String, String)] -> Expectation
expectUnsafeWithLabel expectedLabel participantsAndLocals =
  case parseAsContextGraph participantsAndLocals of
    Left err -> expectationFailure err
    Right cg ->
      checkSafety cg `shouldSatisfy` hasMissingLabel expectedLabel

hasMissingLabel :: Label -> Either [SafetyError] () -> Bool
hasMissingLabel expectedLabel (Left errs) =
  any (\err -> seLabel err == expectedLabel) errs
hasMissingLabel _ (Right ()) = False

parseAsContextGraph :: [(String, String)] -> Either String ContextGraph
parseAsContextGraph participantLocals = do
  locals <- mapM parseOne participantLocals
  pure (buildContextGraph locals)
  where
    parseOne (participantName, localSrc) =
      case parseLocalTypeChecked localSrc of
        Left err ->
          Left
            ( "Local type for participant "
                ++ show participantName
                ++ " failed to parse/check:\n"
                ++ err
            )
        Right lt ->
          Right (Participant participantName, buildLocalGraph lt)
