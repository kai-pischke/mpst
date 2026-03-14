module MergeSpec (spec) where

import Automata (LocalGraph, buildLocalGraph, localGraphToType)
import Merge (fullMerge, plainMerge)
import Syntax
  ( LocalType
  , alphaEqLocalType
  , normalizeLocalBranchOrder
  , parseLocalTypeChecked
  )
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec =
  describe "Merge operators" $ do
    describe "Plain merge" $ do
      it "[MERGE-PLAIN-001] accepts isomorphic graphs" $
        expectPlainMergeAs
          "q ! {l1: end, l2: q ? {k: end}}"
          "q ! {l2: q ? {k: end}, l1: end}"
          "q ! {l1: end, l2: q ? {k: end}}"

      it "[MERGE-PLAIN-002] rejects graphs with different outgoing labels" $
        expectPlainMergeFails
          "q ! {l1: end}"
          "q ! {l2: end}"

      it "[MERGE-PLAIN-003] rejects send vs receive mismatch" $
        expectPlainMergeFails
          "q ! {l1: end}"
          "q ? {l1: end}"

    describe "Full merge" $ do
      it "[MERGE-FULL-001] unions receive labels" $
        expectFullMergeAs
          "q ? {l1: end}"
          "q ? {l1: end, l2: end}"
          "q ? {l1: end, l2: end}"

      it "[MERGE-FULL-002] rejects send nodes with different labels" $
        expectFullMergeFails
          "q ! {l1: end, l3: end}"
          "q ! {l2: end, l3: end}"

      it "[MERGE-FULL-003] recursively merges common receive branches" $
        expectFullMergeAs
          "q ? {l1: r ! {x: end}, l2: end}"
          "q ? {l1: r ! {x: end}, l3: end}"
          "q ? {l1: r ! {x: end}, l2: end, l3: end}"

expectPlainMergeAs :: String -> String -> String -> Expectation
expectPlainMergeAs leftSrc rightSrc expectedSrc =
  case (parseLocalGraph leftSrc, parseLocalGraph rightSrc) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftGraph, Right rightGraph) ->
      case plainMerge leftGraph rightGraph of
        Nothing ->
          expectationFailure "plainMerge failed unexpectedly."
        Just mergedGraph ->
          expectGraphAsLocalType mergedGraph expectedSrc

expectPlainMergeFails :: String -> String -> Expectation
expectPlainMergeFails leftSrc rightSrc =
  case (parseLocalGraph leftSrc, parseLocalGraph rightSrc) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftGraph, Right rightGraph) ->
      plainMerge leftGraph rightGraph `shouldBe` Nothing

expectFullMergeAs :: String -> String -> String -> Expectation
expectFullMergeAs leftSrc rightSrc expectedSrc =
  case (parseLocalGraph leftSrc, parseLocalGraph rightSrc) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftGraph, Right rightGraph) ->
      case fullMerge leftGraph rightGraph of
        Nothing ->
          expectationFailure "fullMerge failed unexpectedly."
        Just mergedGraph ->
          expectGraphAsLocalType mergedGraph expectedSrc

expectFullMergeFails :: String -> String -> Expectation
expectFullMergeFails leftSrc rightSrc =
  case (parseLocalGraph leftSrc, parseLocalGraph rightSrc) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftGraph, Right rightGraph) ->
      fullMerge leftGraph rightGraph `shouldBe` Nothing

expectGraphAsLocalType :: LocalGraph -> String -> Expectation
expectGraphAsLocalType graph expectedSrc =
  case (localGraphToType graph, parseLocalTypeChecked expectedSrc) of
    (Left reconErr, _) ->
      expectationFailure
        ( "Merge produced invalid local graph: "
            ++ show reconErr
        )
    (_, Left parseErr) ->
      expectationFailure
        ( "Expected local type parse/check failed:\n"
            ++ parseErr
        )
    (Right actualType, Right expectedType) ->
      sameLocalType actualType expectedType `shouldBe` True

parseLocalGraph :: String -> Either String LocalGraph
parseLocalGraph src =
  case parseLocalTypeChecked src of
    Left err ->
      Left
        ( "Failed to parse/check local type:\n"
            ++ src
            ++ "\nError:\n"
            ++ err
        )
    Right lt ->
      Right (buildLocalGraph lt)

sameLocalType :: LocalType -> LocalType -> Bool
sameLocalType actual expected =
  alphaEqLocalType
    (normalizeLocalBranchOrder actual)
    (normalizeLocalBranchOrder expected)
