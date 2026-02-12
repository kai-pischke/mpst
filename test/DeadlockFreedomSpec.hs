module DeadlockFreedomSpec (spec) where

import Automata (ContextEdgeLabel(..), ContextGraph, buildContextGraph, buildLocalGraph)
import qualified Data.Set as Set
import DeadlockFreedom (DeadlockFreedomError(..), checkDeadlockFreedom)
import Syntax (Participant(..), parseLocalTypeChecked)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "Deadlock freedom" $ do
    it "[DLF-001] accepts a simple 2-party protocol" $
      expectDeadlockFree
        [ ("p", "q ! {l: end}")
        , ("q", "p ? {l: end}")
        ]
    it "[DLF-002] rejects reachable terminal states with pending single-sided transitions" $
      expectDeadlockViolation
        [ ("p", "q ! {l1: q ! {l2: end}}")
        , ("q", "p ? {l1: end}")
        ]
    it "[DLF-003] rejects when start state is terminal and single-sided transition is enabled" $
      expectDeadlockViolation
        [ ("p", "q ! {l: end}")
        , ("q", "end")
        ]
    it "[DLF-004] ignores non-reachable branches" $
      expectDeadlockFree
        [ ("p", "q ! {l1: end, l2: q ! {x: end}}")
        , ("q", "p ? {l1: end}")
        ]

expectDeadlockFree :: [(String, String)] -> Expectation
expectDeadlockFree participantsAndLocals =
  case parseAsContextGraph participantsAndLocals of
    Left err -> expectationFailure err
    Right cg -> checkDeadlockFreedom cg `shouldBe` Right ()

expectDeadlockViolation :: [(String, String)] -> Expectation
expectDeadlockViolation participantsAndLocals =
  case parseAsContextGraph participantsAndLocals of
    Left err -> expectationFailure err
    Right cg ->
      checkDeadlockFreedom cg `shouldSatisfy` hasViolation

hasViolation :: Either [DeadlockFreedomError] () -> Bool
hasViolation (Left errs) =
  not (null errs)
    && any hasSingleEvidence errs
  where
    hasSingleEvidence err =
      any isSingle (Set.toList (dfEnabledSingles err))
    isSingle ContextSingleEdge{} = True
    isSingle _ = False
hasViolation (Right ()) = False

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
