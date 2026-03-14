module InferSpec (spec) where

import Automata (buildLocalGraph)
import Data.Either (isLeft, isRight)
import Infer (infer)
import Subtyping (checkLocalSubtype)
import Syntax
  ( alphaEqLocalType
  , normalizeLocalBranchOrder
  , parseLocalTypeChecked
  , parseProcessChecked
  , renderLocalType
  )
import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( classify
  , counterexample
  , discard
  , forAll
  , (===)
  )
import TestGenerators (canonicalProcess, genWellFormedLocal, genWellFormedProcess)
import Typecheck (typecheck)

spec :: Spec
spec = describe "7) Type Inference" $ do
  describe "Basic inference" $ do
    it "[INF-001] 0 infers to end" $
      expectInfer "0" "end"

    it "[INF-002] p ! l . 0 infers to p ! {l: end}" $
      expectInfer "p ! l . 0" "p ! {l: end}"

    it "[INF-003] p ? {l: 0} infers to p ? {l: end}" $
      expectInfer "p ? {l: 0}" "p ? {l: end}"

    it "[INF-004] p ? {l: 0, m: 0} infers to p ? {l: end, m: end}" $
      expectInfer "p ? {l: 0, m: 0}" "p ? {l: end, m: end}"

  describe "Recursive inference" $ do
    it "[INF-010] rec X . p ! l . X infers to rec t . p ! {l: t}" $
      expectInfer "rec X . p ! l . X" "rec t . p ! {l: t}"

    it "[INF-011] rec X . p ? {go: X, stop: 0} infers to rec t . p ? {go: t, stop: end}" $
      expectInfer
        "rec X . p ? {go: X, stop: 0}"
        "rec t . p ? {go: t, stop: end}"

  describe "Conditional inference" $ do
    it "[INF-020] if true then 0 else 0 infers to end" $
      expectInfer "if true then 0 else 0" "end"

    it "[INF-021] send merge (union)" $
      expectInfer
        "if b then p ! l . 0 else p ! m . 0"
        "p ! {l: end, m: end}"

    it "[INF-022] same label merge" $
      expectInfer
        "if b then p ! l . 0 else p ! l . 0"
        "p ! {l: end}"

    it "[INF-023] recv merge (intersection)" $
      expectInfer
        "if b then p ? {a: 0, b: 0} else p ? {a: 0, c: 0}"
        "p ? {a: end}"

  describe "Combined recursion + conditional" $ do
    it "[INF-030] rec + conditional" $
      expectInfer
        "rec X . if b then p ! l . X else p ! m . 0"
        "rec t . p ! {l: t, m: end}"

  describe "Complex protocols" $ do
    it "[INF-040] ping-pong" $
      expectInfer
        "rec X . p ? {ping: p ! pong . X, quit: 0}"
        "rec t . p ? {ping: p ! {pong: t}, quit: end}"

  describe "Roundtrip (infer then typecheck)" $ do
    it "[INF-050] 0 roundtrips" $
      expectRoundtrip "0"

    it "[INF-051] p ! l . 0 roundtrips" $
      expectRoundtrip "p ! l . 0"

    it "[INF-052] rec X . p ! l . X roundtrips" $
      expectRoundtrip "rec X . p ! l . X"

    it "[INF-053] rec X . p ? {go: X, stop: 0} roundtrips" $
      expectRoundtrip "rec X . p ? {go: X, stop: 0}"

    it "[INF-054] rec X . p ? {ping: p ! pong . X, quit: 0} roundtrips" $
      expectRoundtrip "rec X . p ? {ping: p ! pong . X, quit: 0}"

  describe "Recursive merge (conditional with rec in both branches)" $ do
    it "[INF-070] identical recursive sends merge" $
      expectInfer
        "if b then rec X . p ! l . X else rec Y . p ! l . Y"
        "rec t . p ! {l: t}"

    it "[INF-071] same-structure recursive sends with different labels (union)" $
      expectInfer
        "if b then rec X . p ! l . X else rec Y . p ! m . Y"
        "rec t . p ! {l: t, m: t}"

    it "[INF-072] same-structure recursive recvs with overlapping labels (intersection)" $
      expectInfer
        "if b then rec X . p ? {a: X, b: X} else rec Y . p ? {a: Y, c: Y}"
        "rec t . p ? {a: t}"

    it "[INF-073] recursive merge with different continuations per label" $
      expectInfer
        "if b then rec X . p ? {a: p ! l . X, b: X} else rec Y . p ? {a: p ! m . Y, b: Y}"
        "rec t . p ? {a: p ! {l: t, m: t}, b: t}"

    it "[INF-074] identical recursive sends roundtrip" $
      expectRoundtrip "if b then rec X . p ! l . X else rec Y . p ! l . Y"

    it "[INF-075] recursive send union roundtrip" $
      expectRoundtrip "if b then rec X . p ! l . X else rec Y . p ! m . Y"

  describe "Deeper nesting" $ do
    it "[INF-080] nested send then recv" $
      expectInfer
        "p ! l . q ? {a: q ! m . 0, b: 0}"
        "p ! {l: q ? {a: q ! {m: end}, b: end}}"

    it "[INF-081] nested conditionals (three-way send union)" $
      expectInfer
        "if a then (if b then p ! l . 0 else p ! m . 0) else p ! n . 0"
        "p ! {l: end, m: end, n: end}"

    it "[INF-082] conditional inside recv branch" $
      expectInfer
        "p ? {a: if b then p ! l . 0 else p ! m . 0, c: 0}"
        "p ? {a: p ! {l: end, m: end}, c: end}"

    it "[INF-083] multi-participant recursive chain" $
      expectInfer
        "rec X . p ? {req: q ! resp . X, done: 0}"
        "rec t . p ? {req: q ! {resp: t}, done: end}"

    it "[INF-084] conditional inside rec with recv merge" $
      expectInfer
        "rec X . if b then p ? {a: X, b: X} else p ? {a: X, c: X}"
        "rec t . p ? {a: t}"

    it "[INF-085] deeply nested send roundtrip" $
      expectRoundtrip "p ! l . q ? {a: q ! m . 0, b: 0}"

    it "[INF-086] multi-participant recursive roundtrip" $
      expectRoundtrip "rec X . p ? {req: q ! resp . X, done: 0}"

  describe "Different recursion depths (graph-based solver handles these)" $ do
    it "[INF-090] period-2 vs period-3 recursion roundtrips" $
      expectRoundtrip
        "if b then rec X . p ? {l1: p ? {l1: p ? {l1: X, l2: X}}} else rec Y . p ? {l1: p ? {l1: Y, l2: Y}}"

    it "[INF-091] period-1 vs period-2 recursion roundtrips" $
      expectRoundtrip
        "if b then rec X . p ! l . X else rec Y . p ! l . p ! l . Y"

    it "[INF-092] same labels different order (period-2 cycle)" $
      expectRoundtrip
        "if b then rec X . p ! l . p ! m . X else rec Y . p ! m . p ! l . Y"

  describe "Powerset construction (composite NodeKeys)" $ do
    it "[INF-093] same send label, different continuations (non-recursive)" $
      expectInfer
        "if b then p ! l . q ! a . 0 else p ! l . q ! b . 0"
        "p ! {l: q ! {a: end, b: end}}"

    it "[INF-094] recv intersection with continuation divergence" $
      expectInfer
        "if b then p ? {a: q ! l . 0, b: 0} else p ? {a: q ! m . 0, c: 0}"
        "p ? {a: q ! {l: end, m: end}}"

    it "[INF-095] recv intersection, multiple retained labels, different continuations" $
      expectInfer
        "if b then p ? {a: q ! l . 0, b: q ! m . 0, c: 0} else p ? {a: q ! n . 0, b: q ! m . 0, d: 0}"
        "p ? {a: q ! {l: end, n: end}, b: q ! {m: end}}"

  describe "Degenerate and nested recursion" $ do
    it "[INF-097] nested rec with unused outer variable" $
      expectInfer
        "rec X . rec Y . p ! l . Y"
        "rec t . p ! {l: t}"

    it "[INF-098] pseudo-mutual recursion via nesting (alternating actions)" $
      expectInfer
        "rec X . p ! l . rec Y . p ! m . X"
        "rec t . p ! {l: p ! {m: t}}"

    it "[INF-099] pseudo-mutual recursion roundtrip" $
      expectRoundtrip "rec X . p ! l . rec Y . p ! m . X"

  describe "Three-way recursive merge" $ do
    it "[INF-100] three recursive branches merge (send union)" $
      expectInfer
        "if a then rec X . p ! l . X else (if b then rec Y . p ! m . Y else rec Z . p ! n . Z)"
        "rec t . p ! {l: t, m: t, n: t}"

    it "[INF-101] three recursive branches roundtrip" $
      expectRoundtrip
        "if a then rec X . p ! l . X else (if b then rec Y . p ! m . Y else rec Z . p ! n . Z)"

  describe "Error cases" $ do
    it "[INF-060] send/recv mismatch is an error" $
      expectInferFail "if b then p ! l . 0 else p ? {l: 0}"

    it "[INF-061] participant mismatch is an error" $
      expectInferFail "if b then p ! l . 0 else q ! m . 0"

    it "[INF-062] recv intersection with disjoint labels is an error" $
      expectInferFail "if b then p ? {a: 0} else p ? {b: 0}"

    it "[INF-063] send vs end mismatch is an error" $
      expectInferFail "if b then p ! l . 0 else 0"

    it "[INF-064] recursive vs non-recursive same label (send+end in composite node)" $
      expectInferFail "if b then rec X . p ! l . X else p ! l . 0"

    it "[INF-065] recursive send/recv mismatch" $
      expectInferFail "if b then rec X . p ! l . X else rec Y . p ? {l: Y}"

    it "[INF-066] recursive participant mismatch" $
      expectInferFail "if b then rec X . p ! l . X else rec Y . q ! m . Y"

  describe "Property-based inference tests" $ do
    prop "[INF-PROP-001] inferred type always typechecks (soundness)" $
      forAll genWellFormedProcess $ \proc ->
        classify (isRight (infer proc)) "inference-succeeds" $
        case infer proc of
          Left _  -> discard
          Right t -> counterexample (renderLocalType t) $
                       typecheck proc t === Right ()

    prop "[INF-PROP-002] inferred type is principal (subtype of any valid typing)" $
      forAll genWellFormedLocal $ \localType ->
        let proc = canonicalProcess localType
        in classify (isRight (infer proc)) "inference-succeeds" $
           case infer proc of
             Left _  -> discard
             Right inferredType ->
               counterexample ("Inferred: " ++ renderLocalType inferredType
                              ++ "\nOriginal: " ++ renderLocalType localType) $
                 checkLocalSubtype
                   (buildLocalGraph inferredType)
                   (buildLocalGraph localType) === Right ()

------------------------------------------------------------------------
-- Test helpers
------------------------------------------------------------------------

expectInfer :: String -> String -> IO ()
expectInfer procSrc typeSrc =
  case parseProcessChecked procSrc of
    Left err -> expectationFailure ("Parse process failed: " ++ err)
    Right proc ->
      case parseLocalTypeChecked typeSrc of
        Left err -> expectationFailure ("Parse type failed: " ++ err)
        Right expected ->
          case infer proc of
            Left errs -> expectationFailure ("Inference failed: " ++ show errs)
            Right actual ->
              let normActual   = normalizeLocalBranchOrder actual
                  normExpected = normalizeLocalBranchOrder expected
              in if alphaEqLocalType normActual normExpected
                   then pure ()
                   else expectationFailure $
                     "Inferred type does not match expected.\n"
                     ++ "  Expected: " ++ renderLocalType normExpected ++ "\n"
                     ++ "  Actual:   " ++ renderLocalType normActual

expectInferFail :: String -> IO ()
expectInferFail procSrc =
  case parseProcessChecked procSrc of
    Left err -> expectationFailure ("Parse failed unexpectedly: " ++ err)
    Right proc ->
      infer proc `shouldSatisfy` isLeft

expectRoundtrip :: String -> IO ()
expectRoundtrip procSrc =
  case parseProcessChecked procSrc of
    Left err -> expectationFailure ("Parse process failed: " ++ err)
    Right proc ->
      case infer proc of
        Left errs -> expectationFailure ("Inference failed: " ++ show errs)
        Right inferredType ->
          typecheck proc inferredType `shouldSatisfy` isRight
