module SynthesiseSpec (spec) where

import Automata (ContextGraph, GlobalGraph, LocalGraph, buildContextGraph, buildLocalGraph, globalGraphToType)
import qualified Data.Map.Strict as Map
import Project (projectCoinductiveFull)
import Subtyping (checkContextSubtype)
import Synthesise (synthesise)
import Syntax (Participant(..), parseLocalTypeChecked, renderGlobalType)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec =
  describe "Synthesis" $ do
    describe "Unit tests" $ do
      it "[SYNTH-001] two end participants synthesise to end" $
        expectSynthGlobal
          [("p", "end"), ("q", "end")]
          "end"

      it "[SYNTH-002] single message synthesises correctly" $
        expectSynthGlobal
          [("p", "q ! {l: end}"), ("q", "p ? {l: end}")]
          "p -> q {l: end}"

      it "[SYNTH-003] two-branch message synthesises correctly" $
        expectSynthGlobal
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l2: end}")]
          "p -> q {l1: end, l2: end}"

      it "[SYNTH-004] subtype receiver synthesises correctly" $
        expectSynthGlobal
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l2: end, l3: end}")]
          "p -> q {l1: end, l2: end}"
      
      -- It is not too hard to synthesise a global type from two parallel communicating pairs 
      it "[SYNTH-005] synthesise parallel components properly" $
        expectSynthGlobal
          [("a", "b ! {l1: end, l2: end}"), ("b", "a ? {l1: end, l2: end, l3: end}"), 
           ("c", "d ! {l1: end, l2: end}"), ("d", "c ? {l1: end, l2: end, l3: end}")]
          "a -> b {l1: c -> d {l1: end, l2: end}, l2: c -> d {l1: end, l2: end}}"
      
      -- TODO: make tests agnostic to binder name 
      -- A bit harder is the recursive case when we need interleaving 
      it "[SYNTH-006] synthesises recursive parallel components properly" $
        expectSynthGlobal
          [("a", "rec t . b ! {l1: end, l2: t}"), ("b", "rec t . a ? {l1: end, l2: t, l3: end}"), 
           ("c", "rec t . d ! {l1: end, l2: t}"), ("d", "rec t . c ? {l1: end, l2: t, l3: end}")]
          "rec t2 . a -> b {l1: rec t3 . c -> d {l1: end, l2: t3}\n, l2: c -> d {l1: rec t1 . a -> b {l1: end, l2: t1}, l2: t2}}"

      it "[SYNTH-007] recursive protocol satisfies roundtrip" $
        expectSynthRoundtrips
          [("p", "rec t . q ! {l: t}"), ("q", "rec t . p ? {l: t}")]

      it "[SYNTH-008] 3-party protocol satisfies roundtrip" $
        expectSynthRoundtrips
          [("p", "q ! {l: end}"), ("q", "p ? {l: r ! {m: end}}"), ("r", "q ? {m: end}")]

      it "[SYNTH-009] branch+recursion satisfies roundtrip" $
        expectSynthRoundtrips
          [("p", "rec t . q ! {go: t, stop: end}"), ("q", "rec t . p ? {go: t, stop: end}")]

    describe "Completeness property" $ do
      it "[SYNTH-SUPER-001] end context satisfies the property" $
        expectSupertypeProperty
          [("p", "end"), ("q", "end")]

      it "[SYNTH-SUPER-002] single message satisfies the property" $
        expectSupertypeProperty
          [("p", "q ! {l: end}"), ("q", "p ? {l: end}")]

      it "[SYNTH-SUPER-003] subtype receiver satisfies property" $
        expectSupertypeProperty
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l2: end, l3: end}")]

      it "[SYNTH-SUPER-004] recursive protocol satisfies supertype property" $
        expectSupertypeProperty
          [("p", "rec t . q ! {l: t}"), ("q", "rec t . p ? {l: t}")]

      it "[SYNTH-SUPER-005] 3-party satisfies supertype property" $
        expectSupertypeProperty
          [("p", "q ! {l: end}"), ("q", "p ? {l: r ! {m: end}}"), ("r", "q ? {m: end}")]

      it "[SYNTH-SUPER-006] branch+recursion satisfies supertype property" $
        expectSupertypeProperty
          [("p", "rec t . q ! {go: t, stop: end}"), ("q", "rec t . p ? {go: t, stop: end}")]


-- Helpers

buildCtx :: [(String, String)] -> Either String ContextGraph
buildCtx defs = do
  entries <- mapM parseEntry defs
  pure (buildContextGraph entries)
  where
    parseEntry (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left ("Failed to parse local type for " ++ name ++ ": " ++ err)
        Right lt -> Right (Participant name, buildLocalGraph lt)

expectSynthGlobal :: [(String, String)] -> String -> IO ()
expectSynthGlobal defs expectedStr =
  case buildCtx defs of
    Left err -> expectationFailure ("Context build failed: " ++ err)
    Right ctx ->
      case synthesise ctx of
        Left synthErr -> expectationFailure ("Synthesis failed: " ++ show synthErr)
        Right gg ->
          case globalGraphToType gg of
            Left reconErr -> expectationFailure ("Global graph to type failed: " ++ show reconErr)
            Right gType ->
              renderGlobalType gType `shouldBe` expectedStr

expectSynthRoundtrips :: [(String, String)] -> IO ()
expectSynthRoundtrips defs =
  case buildCtx defs of
    Left err -> expectationFailure ("Context build failed: " ++ err)
    Right ctx ->
      case synthesise ctx of
        Left synthErr -> expectationFailure ("Synthesis failed: " ++ show synthErr)
        Right gg ->
          case globalGraphToType gg of
            Left reconErr -> expectationFailure ("Global graph to type failed: " ++ show reconErr)
            Right _ -> pure ()

expectSupertypeProperty :: [(String, String)] -> IO ()
expectSupertypeProperty defs =
  case buildCtx defs of
    Left err -> expectationFailure ("Context build failed: " ++ err)
    Right ctx ->
      case synthesise ctx of
        Left synthErr -> expectationFailure ("Synthesis failed: " ++ show synthErr)
        Right gg -> do
          -- Build the original local graph maps.
          let originalLocals = buildOriginalLocalGraphs defs
          case originalLocals of
            Left err -> expectationFailure ("Original local graph build failed: " ++ err)
            Right origMap -> do
              -- Project the synthesised global graph for each participant.
              let participants = Map.keys origMap
              projectedMap <- buildProjectedLocalGraphs gg participants
              case projectedMap of
                Left err -> expectationFailure err
                Right projMap ->
                  case checkContextSubtype origMap projMap of
                    Right () -> pure ()
                    Left errs ->
                      expectationFailure
                        ( "Supertype property violated: original context is NOT a subtype of projected context.\n"
                            ++ show errs
                        )

buildOriginalLocalGraphs :: [(String, String)] -> Either String (Map.Map Participant LocalGraph)
buildOriginalLocalGraphs defs = do
  entries <- mapM parseEntry defs
  pure (Map.fromList entries)
  where
    parseEntry (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left ("Failed to parse local type for " ++ name ++ ": " ++ err)
        Right lt -> Right (Participant name, buildLocalGraph lt)

buildProjectedLocalGraphs :: GlobalGraph -> [Participant] -> IO (Either String (Map.Map Participant LocalGraph))
buildProjectedLocalGraphs gg participants =
  case mapM projectOne participants of
    Left err -> pure (Left err)
    Right entries -> pure (Right (Map.fromList entries))
  where
    projectOne p =
      case projectCoinductiveFull gg p of
        Left projErr ->
          Left ("Projection failed for " ++ show p ++ ": " ++ show projErr)
        Right lg ->
          Right (p, lg)
