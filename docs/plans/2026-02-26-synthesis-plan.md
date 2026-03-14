# GlobalGraph Synthesis Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Synthesise a GlobalGraph from a ContextGraph by traversing sync edges with round-robin participant priority.

**Architecture:** New `Synthesise` module with a recursive traversal using `State SynthState`. Finds the next send-active participant (cycling from a priority index), creates global nodes/edges, and detects recursion via an env mapping `(Participant, ContextVertex) -> GlobalVertex`. Empty RecVarHints — existing `completeGlobalHints` fills them in.

**Tech Stack:** Haskell, `Data.Graph`, `Data.Map`, `Control.Monad.State.Strict`, hspec for tests.

---

### Task 1: Add Synthesise to package.yaml

**Files:**
- Modify: `package.yaml`

**Step 1: Add module declarations**

Add `Synthesise` to `library.exposed-modules` (after `Subtyping`). Add `SynthesiseSpec` to `tests.mpst-test.other-modules`.

**Step 2: Verify it still builds (no new module yet, deferred)**

---

### Task 2: Write unit tests for simple synthesis cases

**Files:**
- Create: `test/SynthesiseSpec.hs`

**Step 1: Write the failing tests**

```haskell
module SynthesiseSpec (spec) where

import Automata
  ( ContextGraph
  , GlobalGraph
  , buildContextGraph
  , buildLocalGraph
  , globalGraphToType
  , localGraphToType
  )
import qualified Data.Map.Strict as Map
import Project (projectCoinductiveFull)
import Subtyping (checkContextSubtype)
import Synthesise (SynthesisError, synthesise)
import Syntax
  ( GlobalType(..)
  , Label(..)
  , LocalType(..)
  , Participant(..)
  , TypeVar(..)
  , parseLocalTypeChecked
  , renderGlobalType
  )
import Test.Hspec
  ( Spec
  , describe
  , expectationFailure
  , it
  , shouldBe
  , shouldSatisfy
  )
import Data.Either (isRight)

spec :: Spec
spec =
  describe "Synthesis" $ do
    describe "unit tests" $ do
      it "synthesises end for a terminated context" $
        expectSynthGlobal
          [("p", "end"), ("q", "end")]
          "end"

      it "synthesises a simple 2-party send/receive" $
        expectSynthGlobal
          [("p", "q ! {l: end}"), ("q", "p ? {l: end}")]
          "p -> q {l: end}"

      it "synthesises a 2-party protocol with multiple branches" $
        expectSynthGlobal
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l2: end}")]
          "p -> q {l1: end, l2: end}"

      it "synthesises a recursive 2-party protocol" $
        expectSynthRoundtrips
          [("p", "rec t . q ! {l: t}"), ("q", "rec t . p ? {l: t}")]

      it "synthesises a 3-party chain" $
        expectSynthRoundtrips
          [ ("p", "q ! {l: end}")
          , ("q", "p ? {l: r ! {m: end}}")
          , ("r", "q ? {m: end}")
          ]

      it "synthesises a 2-party protocol with branching and recursion" $
        expectSynthRoundtrips
          [ ("p", "rec t . q ! {go: t, stop: end}")
          , ("q", "rec t . p ? {go: t, stop: end}")
          ]

    describe "supertype property" $ do
      it "synthesised global projects onto supertype of original (simple loop)" $
        expectSupertypeProperty
          [("p", "rec t . q ! {a: t}"), ("q", "rec t . p ? {a: t}")]

      it "synthesised global projects onto supertype of original (3-party)" $
        expectSupertypeProperty
          [ ("p", "q ! {l: end}")
          , ("q", "p ? {l: r ! {m: end}}")
          , ("r", "q ? {m: end}")
          ]

      it "synthesised global projects onto supertype of original (branch+rec)" $
        expectSupertypeProperty
          [ ("p", "rec t . q ! {go: t, stop: end}")
          , ("q", "rec t . p ? {go: t, stop: end}")
          ]

-- | Parse local types, build context graph, synthesise, convert to global type string.
expectSynthGlobal :: [(String, String)] -> String -> IO ()
expectSynthGlobal pairs expectedGlobal =
  case buildCtx pairs of
    Left err -> expectationFailure err
    Right cg ->
      case synthesise cg of
        Left synthErr -> expectationFailure ("synthesis failed: " ++ show synthErr)
        Right gg ->
          case globalGraphToType gg of
            Left gtErr -> expectationFailure ("globalGraphToType failed: " ++ show gtErr)
            Right gt -> renderGlobalType gt `shouldBe` expectedGlobal

-- | Parse, synthesise, and verify the result can be converted back to a type.
expectSynthRoundtrips :: [(String, String)] -> IO ()
expectSynthRoundtrips pairs =
  case buildCtx pairs of
    Left err -> expectationFailure err
    Right cg ->
      case synthesise cg of
        Left synthErr -> expectationFailure ("synthesis failed: " ++ show synthErr)
        Right gg ->
          globalGraphToType gg `shouldSatisfy` isRight

-- | The key correctness property: synthesised global projects onto a supertype context.
expectSupertypeProperty :: [(String, String)] -> IO ()
expectSupertypeProperty pairs =
  case buildCtx pairs of
    Left err -> expectationFailure err
    Right cg ->
      case synthesise cg of
        Left synthErr -> expectationFailure ("synthesis failed: " ++ show synthErr)
        Right gg -> do
          let participants = contextParticipants pairs
          projectedCtx <- projectAll gg participants
          let originalCtx = buildLocalGraphCtx pairs
          case (projectedCtx, originalCtx) of
            (Right proj, Right orig) ->
              checkContextSubtype orig proj `shouldBe` Right ()
            (Left projErr, _) -> expectationFailure ("projection failed: " ++ show projErr)
            (_, Left origErr) -> expectationFailure ("original ctx build failed: " ++ origErr)

buildCtx :: [(String, String)] -> Either String ContextGraph
buildCtx pairs = do
  locals <- mapM parsePair pairs
  pure (buildContextGraph locals)
  where
    parsePair (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left ("parse error for " ++ name ++ ": " ++ err)
        Right lt -> Right (Participant name, buildLocalGraph lt)

contextParticipants :: [(String, String)] -> [Participant]
contextParticipants = map (Participant . fst)

projectAll :: GlobalGraph -> [Participant] -> IO (Either String (Map.Map Participant Automata.LocalGraph))
projectAll gg participants =
  pure $ foldl step (Right Map.empty) participants
  where
    step (Left err) _ = Left err
    step (Right acc) p =
      case projectCoinductiveFull gg p of
        Left err -> Left (show err)
        Right lg -> Right (Map.insert p lg acc)

buildLocalGraphCtx :: [(String, String)] -> Either String (Map.Map Participant Automata.LocalGraph)
buildLocalGraphCtx pairs =
  foldl step (Right Map.empty) pairs
  where
    step (Left err) _ = Left err
    step (Right acc) (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left err
        Right lt -> Right (Map.insert (Participant name) (buildLocalGraph lt) acc)
```

**Step 2: Run tests to verify they fail**

Run: `stack test --test-arguments="--match Synthesis" 2>&1 | tail -10`
Expected: Compilation error — `Synthesise` module not found.

---

### Task 3: Implement the synthesis algorithm

**Files:**
- Create: `src/Synthesise.hs`

**Step 1: Write the implementation**

```haskell
module Synthesise
  ( SynthesisError(..)
  , synthesise
  ) where

import Automata
  ( ContextEdgeLabel(..)
  , ContextGraph(..)
  , GlobalEdgeLabel(..)
  , GlobalGraph(..)
  , GlobalNode(..)
  , LocalDirection(..)
  , RecVarHints(..)
  )
import Data.Array (array)
import Data.Foldable (foldl')
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST (Label, Participant)

data SynthesisError
  = MultipleReceivers G.Vertex Participant
  | InternalError String
  deriving (Eq, Show)

data SynthState = SynthState
  { ssNext :: !G.Vertex
  , ssNodes :: Map.Map G.Vertex GlobalNode
  , ssEdges :: [(G.Edge, GlobalEdgeLabel)]
  , ssEnv :: Map.Map (Participant, G.Vertex) G.Vertex
  }

emptySynthState :: SynthState
emptySynthState = SynthState 0 Map.empty [] Map.empty

synthesise :: ContextGraph -> Either SynthesisError GlobalGraph
synthesise cg =
  let participants = cgParticipants cg
      outgoing = contextOutgoing cg
      (startVertex, finalState) = runSynth (synthNode cg outgoing participants 0 (cgStart cg)) emptySynthState
   in Right (finaliseGlobal startVertex finalState)

runSynth :: (SynthState -> Either SynthesisError (G.Vertex, SynthState)) -> SynthState -> (G.Vertex, SynthState)
runSynth action st =
  case action st of
    Left err -> error ("Synthesis internal error: " ++ show err)
    Right result -> result

-- but we should propagate errors properly:
synthesise cg =
  let participants = cgParticipants cg
      outgoing = contextOutgoing cg
   in do
        (startVertex, finalState) <- synthNode cg outgoing participants 0 (cgStart cg) emptySynthState
        Right (finaliseGlobal startVertex finalState)

synthNode ::
  ContextGraph ->
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  [Participant] ->
  Int ->
  G.Vertex ->
  SynthState ->
  Either SynthesisError (G.Vertex, SynthState)
synthNode cg outgoing participants priorityIdx contextVertex st =
  case findSendActive outgoing participants priorityIdx contextVertex of
    Nothing ->
      -- No send-active participant: emit end node
      let v = ssNext st
          st' = st
            { ssNext = v + 1
            , ssNodes = Map.insert v GlobalEndNode (ssNodes st)
            }
       in Right (v, st')
    Just (sender, senderIdx) ->
      case Map.lookup (sender, contextVertex) (ssEnv st) of
        Just existingVertex ->
          -- Back-edge: reuse existing global node
          Right (existingVertex, st)
        Nothing -> do
          -- Fresh node
          let v = ssNext st
              st1 = st
                { ssNext = v + 1
                , ssNodes = Map.insert v GlobalNode (ssNodes st1)
                , ssEnv = Map.insert (sender, contextVertex) v (ssEnv st)
                }
          -- Collect sync edges where sender is active
          let syncEdges = collectSyncEdges outgoing contextVertex sender
          receiver <- uniqueReceiver contextVertex sender syncEdges
          let nextPriority = (senderIdx + 1) `mod` length participants
          -- Recurse for each branch
          st2 <- foldl' (synthBranch cg outgoing participants nextPriority sender receiver v) (Right st1) syncEdges
          pure (v, st2)

-- NOTE: The above has a bug with lazy ssNodes update referencing st1. Let me fix:
```

Actually, let me write the correct implementation directly without the false starts above.

```haskell
module Synthesise
  ( SynthesisError(..)
  , synthesise
  ) where

import Automata
  ( ContextEdgeLabel(..)
  , ContextGraph(..)
  , GlobalEdgeLabel(..)
  , GlobalGraph(..)
  , GlobalNode(..)
  , LocalDirection(..)
  , RecVarHints(..)
  )
import Data.Array (array)
import Data.Foldable (foldl')
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import Syntax.AST (Label, Participant)

data SynthesisError
  = MultipleReceivers G.Vertex Participant
  | InternalError String
  deriving (Eq, Show)

data SynthState = SynthState
  { ssNext :: !G.Vertex
  , ssNodes :: !(Map.Map G.Vertex GlobalNode)
  , ssEdges :: ![(G.Edge, GlobalEdgeLabel)]
  , ssEnv :: !(Map.Map (Participant, G.Vertex) G.Vertex)
  }

synthesise :: ContextGraph -> Either SynthesisError GlobalGraph
synthesise cg = do
  let participants = cgParticipants cg
      outgoing = contextOutgoing cg
      st0 = SynthState 0 Map.empty [] Map.empty
  (start, stFinal) <- synthNode outgoing participants 0 (cgStart cg) st0
  pure (buildResult start stFinal)

synthNode ::
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  [Participant] ->
  Int ->
  G.Vertex ->
  SynthState ->
  Either SynthesisError (G.Vertex, SynthState)
synthNode outgoing participants priorityIdx ctxVertex st =
  case findSendActive outgoing participants priorityIdx ctxVertex of
    Nothing ->
      let v = ssNext st
       in Right
            ( v
            , st
                { ssNext = v + 1
                , ssNodes = Map.insert v GlobalEndNode (ssNodes st)
                }
            )
    Just (sender, senderIdx) ->
      case Map.lookup (sender, ctxVertex) (ssEnv st) of
        Just existing -> Right (existing, st)
        Nothing -> do
          let v = ssNext st
              st1 =
                st
                  { ssNext = v + 1
                  , ssNodes = Map.insert v GlobalNode (ssNodes st)
                  , ssEnv = Map.insert (sender, ctxVertex) v (ssEnv st)
                  }
              syncEdges = collectSyncEdges outgoing ctxVertex sender
          receiver <- uniqueReceiver ctxVertex sender syncEdges
          let nextPriority = (senderIdx + 1) `mod` length participants
          st2 <- foldlM (addBranch outgoing participants nextPriority v sender receiver) st1 syncEdges
          pure (v, st2)

addBranch ::
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  [Participant] ->
  Int ->
  G.Vertex ->
  Participant ->
  Participant ->
  SynthState ->
  (G.Vertex, Label, ContextEdgeLabel) ->
  Either SynthesisError SynthState
addBranch outgoing participants nextPriority fromVertex sender receiver st (targetCtxVertex, label, _) = do
  (targetGlobalVertex, st') <- synthNode outgoing participants nextPriority targetCtxVertex st
  let edgeLabel =
        GlobalEdgeLabel
          { geSender = sender
          , geReceiver = receiver
          , geLabel = label
          , geTargetHints = RecVarHints [] Nothing
          }
  pure
    st'
      { ssEdges = ((fromVertex, targetGlobalVertex), edgeLabel) : ssEdges st'
      }

-- | Find the next send-active participant starting from priorityIdx.
findSendActive ::
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  [Participant] ->
  Int ->
  G.Vertex ->
  Maybe (Participant, Int)
findSendActive outgoing participants startIdx ctxVertex =
  let n = length participants
      indices = take n [startIdx .. startIdx + n - 1]
      edges = Map.findWithDefault [] ctxVertex outgoing
      senders = [ceSender e | (_, e@ContextSyncEdge{}) <- edges]
      sendActiveSet = foldl' (\s p -> Map.insertWith (const id) p () s) Map.empty senders
   in firstMatch participants indices sendActiveSet
  where
    firstMatch _ [] _ = Nothing
    firstMatch ps (i : is) active =
      let p = ps !! (i `mod` length ps)
       in if p `Map.member` active
            then Just (p, i `mod` length ps)
            else firstMatch ps is active

-- | Collect sync edges from a context vertex where a given participant is sender.
collectSyncEdges ::
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  G.Vertex ->
  Participant ->
  [(G.Vertex, Label, ContextEdgeLabel)]
collectSyncEdges outgoing ctxVertex sender =
  [ (target, ceLabel edge, edge)
  | (target, edge@ContextSyncEdge{}) <- Map.findWithDefault [] ctxVertex outgoing
  , ceSender edge == sender
  ]

-- | Verify all sync edges for a sender go to the same receiver.
uniqueReceiver :: G.Vertex -> Participant -> [(G.Vertex, Label, ContextEdgeLabel)] -> Either SynthesisError Participant
uniqueReceiver ctxVertex sender edges =
  case edges of
    [] -> Left (InternalError ("No sync edges for sender " ++ show sender ++ " at vertex " ++ show ctxVertex))
    (_, _, first) : rest ->
      let receiver = ceReceiver first
       in if all (\(_, _, e) -> ceReceiver e == receiver) rest
            then Right receiver
            else Left (MultipleReceivers ctxVertex sender)

-- | Build outgoing edge map from context graph.
contextOutgoing :: ContextGraph -> Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)]
contextOutgoing cg =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(to, lbl)] m) acc labels)
    Map.empty
    (cgEdgeLabels cg)

-- | Build the final GlobalGraph from synthesis state.
buildResult :: G.Vertex -> SynthState -> GlobalGraph
buildResult start st =
  let n = ssNext st
      bounds = (0, n - 1)
      edgePairs = map fst (ssEdges st)
      graph = G.buildG bounds edgePairs
      nodeTable = array bounds (Map.toList (ssNodes st))
      edgeLabels = collectEdgeLabels (ssEdges st)
   in GlobalGraph
        { ggGraph = graph
        , ggStart = start
        , ggNodes = nodeTable
        , ggEdgeLabels = edgeLabels
        , ggStartVarHints = RecVarHints [] Nothing
        }

collectEdgeLabels :: [(G.Edge, GlobalEdgeLabel)] -> Map.Map G.Edge [GlobalEdgeLabel]
collectEdgeLabels = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty

foldlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM _ acc [] = pure acc
foldlM f acc (x : xs) = do
  acc' <- f acc x
  foldlM f acc' xs
```

**Step 2: Run the tests**

Run: `stack test --test-arguments="--match Synthesis" 2>&1 | tail -20`
Expected: Unit tests pass. Some test helper compilation issues may need fixing.

**Step 3: Commit**

```bash
git add src/Synthesise.hs test/SynthesiseSpec.hs package.yaml
git commit -m "add synthesis module"
```

---

### Task 4: Wire into MPST module and test runner

**Files:**
- Modify: `src/MPST.hs`
- Modify: `test/Spec.hs`

**Step 1: Add Synthesise to MPST.hs re-exports**

Add `module Synthesise` to the export list and `import Synthesise` to imports.

**Step 2: Add SynthesiseSpec to test/Spec.hs**

Add `import qualified SynthesiseSpec` and `SynthesiseSpec.spec` to the hspec block.

**Step 3: Run full test suite**

Run: `stack test 2>&1 | tail -20`
Expected: All tests pass.

**Step 4: Commit**

```bash
git add src/MPST.hs test/Spec.hs
git commit -m "wire Synthesise into MPST"
```

---

### Task 5: Final verification

**Step 1: Run full test suite**

Run: `stack test 2>&1`
Expected: All tests pass.

**Step 2: Verify manually via ghci**

```haskell
import qualified Data.Map.Strict as Map
import Automata
import Synthesise
import Syntax
import Project

let Right p = parseLocalTypeChecked "rec t . q ! {a: t}"
let Right q = parseLocalTypeChecked "rec t . p ? {a: t}"
let cg = buildContextGraph [(Participant "p", buildLocalGraph p), (Participant "q", buildLocalGraph q)]
let Right gg = synthesise cg
globalGraphToType gg
```

Expected: Returns a `Right (GRec ...)` representing `rec t . p -> q {a: t}`.
