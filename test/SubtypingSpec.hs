module SubtypingSpec (spec) where

import Automata (ContextGraph, LocalGraph, buildContextGraph, buildLocalGraph)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Liveness (checkLiveness)
import Safety (checkSafety)
import Subtyping (checkContextSubtype, checkLocalSubtype)
import Syntax
  ( LocalType(..)
  , Participant(..)
  , parseLocalTypeChecked
  , unfoldRec
  )
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, (==>), arbitrary, classify, cover, counterexample, forAll)
import TestGenerators (GeneratedContext(..), genContext)

spec :: Spec
spec =
  do
    describe "5) Local subtyping" $ do
      it "[SUB-001] selection is covariant in branch sets" $
        expectSubtype
          "p ! {l1: end}"
          "p ! {l1: end, l2: end}"
      it "[SUB-002] branching is contravariant in branch sets" $
        expectSubtype
          "p ? {l1: end, l2: end}"
          "p ? {l1: end}"
      it "[SUB-003] allows recursion unfolding on the left" $
        expectSubtype
          "rec t . p ! {l1: p ! {l1: t}}"
          "rec t . p ! {l1: t}"
      it "[SUB-004] allows recursion unfolding on the right" $
        expectSubtype
          "rec t . p ! {l1: t}"
          "rec t . p ! {l1: p ! {l1: t}}"
      it "[SUB-005] rejects incompatible action kinds" $
        expectNotSubtype
          "p ! {l1: end}"
          "p ? {l1: end}"
      it "[SUB-006] rejects wrong participant" $
        expectNotSubtype
          "p ! {l1: end}"
          "q ! {l1: end}"

    describe "Context subtyping (pointwise by participant)" $ do
      it "[SUB-CTX-001] accepts when every participant local type is a subtype" $
        expectContextSubtype
          [ ("p", "q ! {l1: end}")
          , ("q", "p ? {l1: end, l2: end}")
          ]
          [ ("p", "q ! {l1: end, l2: end}")
          , ("q", "p ? {l1: end}")
          ]

      it "[SUB-CTX-002] rejects when one participant local type is not a subtype" $
        expectContextNotSubtype
          [ ("p", "q ! {l1: end}")
          , ("q", "p ? {l1: end}")
          ]
          [ ("p", "q ! {l1: end, l2: end}")
          , ("q", "p ? {l1: end, l2: end}")
          ]

      it "[SUB-CTX-003] rejects when participant sets differ" $
        expectContextNotSubtype
          [ ("p", "q ! {l1: end}")
          , ("q", "p ? {l1: end}")
          ]
          [ ("p", "q ! {l1: end}")
          , ("q", "p ? {l1: end}")
          , ("r", "end")
          ]

    -- Properties discussed in:
    -- Thien Udomsrirungruang, Nobuko Yoshida.
    -- "Top-Down or Bottom-Up? Complexity Analyses of Synchronous Multiparty Session Types"
    -- (POPL 2025), https://arxiv.org/abs/2411.07452
    describe "Context subtyping preserves properties (QuickCheck)" $ do
      prop "[SUB-CTX-PROP-001] safety is preserved downward along subtyping" $
        propSafetyPreservedByContextSubtyping

      prop "[SUB-CTX-PROP-002] liveness is preserved downward along subtyping" $
        propLivenessPreservedByContextSubtyping

expectSubtype :: String -> String -> Expectation
expectSubtype leftSrc rightSrc =
  case parseSubtypePair leftSrc rightSrc of
    Left err -> expectationFailure err
    Right (leftGraph, rightGraph) ->
      checkLocalSubtype leftGraph rightGraph `shouldBe` Right ()

expectNotSubtype :: String -> String -> Expectation
expectNotSubtype leftSrc rightSrc =
  case parseSubtypePair leftSrc rightSrc of
    Left err -> expectationFailure err
    Right (leftGraph, rightGraph) ->
      checkLocalSubtype leftGraph rightGraph `shouldSatisfy` isLeft

parseSubtypePair :: String -> String -> Either String (LocalGraph, LocalGraph)
parseSubtypePair leftSrc rightSrc = do
  left <- parseLocalGraph "left (subtype candidate)" leftSrc
  right <- parseLocalGraph "right (supertype candidate)" rightSrc
  pure (left, right)

expectContextSubtype :: [(String, String)] -> [(String, String)] -> Expectation
expectContextSubtype leftDefs rightDefs =
  case (parseContextGraphs "left context" leftDefs, parseContextGraphs "right context" rightDefs) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftCtx, Right rightCtx) ->
      checkContextSubtype leftCtx rightCtx `shouldBe` Right ()

expectContextNotSubtype :: [(String, String)] -> [(String, String)] -> Expectation
expectContextNotSubtype leftDefs rightDefs =
  case (parseContextGraphs "left context" leftDefs, parseContextGraphs "right context" rightDefs) of
    (Left err, _) -> expectationFailure err
    (_, Left err) -> expectationFailure err
    (Right leftCtx, Right rightCtx) ->
      checkContextSubtype leftCtx rightCtx `shouldSatisfy` isLeft

parseContextGraphs ::
  String ->
  [(String, String)] ->
  Either String (Map.Map Participant LocalGraph)
parseContextGraphs side defs = do
  entries <- mapM parseEntry defs
  pure (Map.fromList entries)
  where
    parseEntry (participantName, localSrc) = do
      graph <- parseLocalGraph ("participant " ++ participantName ++ " in " ++ side) localSrc
      pure (Participant participantName, graph)

parseLocalGraph :: String -> String -> Either String LocalGraph
parseLocalGraph side source =
  case parseLocalTypeChecked source of
    Left err ->
      Left
        ( "Local type for "
            ++ side
            ++ " should parse and be well-formed, but failed with:\n"
            ++ err
        )
    Right localType ->
      Right (buildLocalGraph localType)

propSafetyPreservedByContextSubtyping :: Property
propSafetyPreservedByContextSubtyping =
  forAll genContextSubtypePair $ \(leftCtx, rightCtx) ->
    let subtype = checkContextSubtype leftCtx rightCtx
        rightSafe = isSafeContext rightCtx
        leftSafe = isSafeContext leftCtx
     in classify (subtype == Right ()) "context-subtype"
          $ cover 20 rightSafe "supertype-safe"
          $ subtype == Right ()
            ==> counterexample
              ( unlines
                  [ "Expected safety preservation under context subtyping."
                  , "left <: right, right safe, but left unsafe."
                  ]
              )
              (not rightSafe || leftSafe)

propLivenessPreservedByContextSubtyping :: Property
propLivenessPreservedByContextSubtyping =
  forAll genContextSubtypePair $ \(leftCtx, rightCtx) ->
    let subtype = checkContextSubtype leftCtx rightCtx
        rightLive = isLiveContext rightCtx
        leftLive = isLiveContext leftCtx
     in classify (subtype == Right ()) "context-subtype"
          $ cover 10 rightLive "supertype-live"
          $ subtype == Right ()
            ==> counterexample
              ( unlines
                  [ "Expected liveness preservation under context subtyping."
                  , "left <: right, right live, but left not live."
                  ]
              )
              (not rightLive || leftLive)

genContextSubtypePair ::
  Gen
    ( Map.Map Participant LocalGraph
    , Map.Map Participant LocalGraph
    )
genContextSubtypePair = do
  GeneratedContext _ participantsAndLocals _ <- genContext
  rightLocals <- mapM (\(p, lt) -> (,) p <$> genLocalSubtypeVariant lt) participantsAndLocals
  let leftCtx = Map.fromList [(p, buildLocalGraph lt) | (p, lt) <- participantsAndLocals]
      rightCtx = Map.fromList [(p, buildLocalGraph lt) | (p, lt) <- rightLocals]
  pure (leftCtx, rightCtx)

genLocalSubtypeVariant :: LocalType -> Gen LocalType
genLocalSubtypeVariant localType = do
  doUnfold <- arbitrary
  pure $
    if doUnfold
      then unfoldRecOnce localType
      else localType

-- | Unfold the outermost rec, and recursively unfold any nested top-level recs.
unfoldRecOnce :: LocalType -> LocalType
unfoldRecOnce localType =
  case localType of
    LRec _ _ -> unfoldRec localType
    LSend peer branches ->
      LSend peer (fmap (\(lbl, cont) -> (lbl, unfoldRecOnce cont)) branches)
    LRecv peer branches ->
      LRecv peer (fmap (\(lbl, cont) -> (lbl, unfoldRecOnce cont)) branches)
    LPayloadSend peer pt cont ->
      LPayloadSend peer pt (unfoldRecOnce cont)
    LPayloadRecv peer pt cont ->
      LPayloadRecv peer pt (unfoldRecOnce cont)
    LVar tv -> LVar tv
    LEnd -> LEnd

isSafeContext :: Map.Map Participant LocalGraph -> Bool
isSafeContext context =
  checkSafety (contextGraph context) == Right ()

isLiveContext :: Map.Map Participant LocalGraph -> Bool
isLiveContext context =
  checkLiveness (contextGraph context) == Right ()

contextGraph :: Map.Map Participant LocalGraph -> ContextGraph
contextGraph =
  buildContextGraph . Map.toList
