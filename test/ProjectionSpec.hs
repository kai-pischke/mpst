module ProjectionSpec (spec) where

import Automata (GlobalGraph, LocalGraph, buildGlobalGraph, buildLocalGraph, localGraphToType)
import Merge (fullMerge, iso)
import Project (ProjectionResult, projectInductiveFull, projectInductivePlain)
import Syntax
  ( LocalType
  , Participant(..)
  , alphaEqLocalType
  , normalizeLocalBranchOrder
  , parseGlobalTypeChecked
  , parseLocalTypeChecked
  )
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, pendingWith, shouldBe)

spec :: Spec
spec =
  do
    describe "2) Projection algorithms" $ do
      it "[PROJ-IF-001] inductive-full projects a simple choice protocol" $
        expectProjectionAs
          projectInductiveFull
          "r -> s {a: q -> p {l1: end}, b: q -> p {l1: end, l2: end}}"
          "p"
          "q ? {l1: end, l2: end}"
      it "[PROJ-IF-002] inductive-full rejects non-projectable branching" $
        expectProjectionFails
          projectInductiveFull
          "r -> s {a: p -> q {l1: end}, b: p -> q {l2: end}}"
          "p"
      it "[PROJ-IP-001] inductive-plain projects a simple recursive protocol" $
        expectProjectionAs
          projectInductivePlain
          "rec t . r -> s {x: p -> q {l: t}}"
          "p"
          "rec t . q ! {l: t}"
      it "[PROJ-IP-002] inductive-plain preserves branch labels at local endpoints" $
        expectProjectionAs
          projectInductivePlain
          "p -> q {k: r -> s {x: q -> p {ack: end}, y: q -> p {ack: end}}}"
          "p"
          "q ! {k: q ? {ack: end}}"
      it "[PROJ-CF-001] coinductive-full accepts productive recursion" $
        pendingWith "TODO: implement when projectCoinductiveFull is implemented"
      it "[PROJ-CF-002] coinductive-full reports merge incompatibility" $
        pendingWith "TODO: implement when projectCoinductiveFull is implemented"
      it "[PROJ-CP-001] coinductive-plain projects finite acyclic protocols" $
        pendingWith "TODO: implement when projectCoinductivePlain is implemented"
      it "[PROJ-CP-002] coinductive-plain rejects ambiguous participant views" $
        pendingWith "TODO: implement when projectCoinductivePlain is implemented"

    describe "Inductive Merge" $ do
      it "[ISO-001] accepts graphs with identical reachable shape and labels" $
        expectIso
          True
          "q ! {l1: end, l2: q ? {k: end}}"
          "q ! {l2: q ? {k: end}, l1: end}"

      it "[ISO-002] rejects graphs with different outgoing labels" $
        expectIso
          False
          "q ! {l1: end}"
          "q ! {l2: end}"

      it "[ISO-003] rejects send vs recv merge" $
        expectIso
          False
          "q ! {l1: end}"
          "q ? {l1: end}"

    describe "Projection merge helpers" $ do
      it "[MERGE-FULL-001] full merge unions receive labels" $
        expectFullMergeAs
          "q ? {l1: end}"
          "q ? {l1: end, l2: end}"
          "q ? {l1: end, l2: end}"

      it "[MERGE-FULL-002] full merge rejects send nodes with different labels" $
        expectFullMergeFails
          "q ! {l1: end, l3: end}"
          "q ! {l2: end, l3: end}"

      it "[MERGE-FULL-003] full merge recursively merges common receive branches" $
        expectFullMergeAs
          "q ? {l1: r ! {x: end}, l2: end}"
          "q ? {l1: r ! {x: end}, l3: end}"
          "q ? {l1: r ! {x: end}, l2: end, l3: end}"

expectIso :: Bool -> String -> String -> Expectation
expectIso expected leftSrc rightSrc =
  case (parseLocalGraph leftSrc, parseLocalGraph rightSrc) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftGraph, Right rightGraph) ->
      iso leftGraph rightGraph `shouldBe` expected

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
          case (localGraphToType mergedGraph, parseLocalTypeChecked expectedSrc) of
            (Left reconErr, _) ->
              expectationFailure
                ( "fullMerge produced invalid local graph: "
                    ++ show reconErr
                )
            (_, Left parseErr) ->
              expectationFailure
                ( "Expected local type parse/check failed:\n"
                    ++ parseErr
                )
            (Right actualType, Right expectedType) ->
              sameLocalType actualType expectedType `shouldBe` True

expectFullMergeFails :: String -> String -> Expectation
expectFullMergeFails leftSrc rightSrc =
  case (parseLocalGraph leftSrc, parseLocalGraph rightSrc) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftGraph, Right rightGraph) ->
      fullMerge leftGraph rightGraph `shouldBe` Nothing

expectProjectionAs ::
  (GlobalGraph -> Participant -> ProjectionResult) ->
  String ->
  String ->
  String ->
  Expectation
expectProjectionAs projectFn globalSrc participantName expectedLocalSrc =
  case parseGlobalTypeChecked globalSrc of
    Left err ->
      expectationFailure
        ( "Global type parse/check failed:\n"
            ++ err
        )
    Right globalType ->
      case projectFn (buildGlobalGraph globalType) (Participant participantName) of
        Left projectErr ->
          expectationFailure
            ( "Projection failed unexpectedly with: "
                ++ show projectErr
            )
        Right localGraph ->
          case localGraphToType localGraph of
            Left reconErr ->
              expectationFailure
                ( "Projection produced invalid local graph (cannot reconstruct local type): "
                    ++ show reconErr
                )
            Right projectedType ->
              case parseLocalTypeChecked expectedLocalSrc of
                Left expectedErr ->
                  expectationFailure
                    ( "Expected local type parse/check failed:\n"
                        ++ expectedErr
                    )
                Right expectedType ->
                  if sameLocalType projectedType expectedType
                    then pure ()
                    else
                      expectationFailure
                        ( "Projected type mismatch.\nExpected: "
                            ++ show expectedType
                            ++ "\nActual: "
                            ++ show projectedType
                        )

expectProjectionFails ::
  (GlobalGraph -> Participant -> ProjectionResult) ->
  String ->
  String ->
  Expectation
expectProjectionFails projectFn globalSrc participantName =
  case parseGlobalTypeChecked globalSrc of
    Left err ->
      expectationFailure
        ( "Global type parse/check failed:\n"
            ++ err
        )
    Right globalType ->
      case projectFn (buildGlobalGraph globalType) (Participant participantName) of
        Left _ -> pure ()
        Right localGraph ->
          expectationFailure
            ( "Projection unexpectedly succeeded with local graph: "
                ++ show localGraph
            )

sameLocalType :: LocalType -> LocalType -> Bool
sameLocalType actual expected =
  alphaEqLocalType
    (normalizeLocalBranchOrder actual)
    (normalizeLocalBranchOrder expected)
