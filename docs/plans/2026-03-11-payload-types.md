# Payload Send/Receive Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add payload send/receive (`p -> q [int]; G`) as a separate construct alongside existing labeled choices (`p -> q {l1: G1, l2: G2}`).

**Architecture:** Payload messages are semantically distinct from labeled choices: they have a single continuation (no branching), so uninvolved participants cannot use label-set union to merge different payload receives. The implementation adds new AST constructors, automata node/edge types, and extends all algorithms to handle payload nodes. The automata use separate edge maps for branch and payload edges to minimize disruption to existing code.

**Tech Stack:** Haskell, Stack, Megaparsec, prettyprinter, HSpec, QuickCheck

---

## Design Decisions

1. **Separate AST constructors**: `GPayload`, `LPayloadSend`, `LPayloadRecv`, `PSendPayload`, `PRecvPayload`
2. **Payload types**: `PTInt | PTBool | PTUnit` only
3. **No payloads on labels**: the two mechanisms are strictly orthogonal
4. **Payload merge fails** unless same type + compatible continuations (no label-union)
5. **Automata**: Separate edge maps (`ggPayloadEdges`, `lgPayloadEdges`) alongside existing maps; new node constructors (`GlobalPayloadNode`, `LocalPayloadSendNode`, `LocalPayloadRecvNode`)
6. **Builder parameterization**: `Either BranchEdge PayloadEdge` edge type, partitioned at finalization
7. **Process syntax**: `p ! [e] . P` (send), `p ? (x) . P` (receive) — brackets/parens disambiguate from labels

## Key Semantic Insight

Labeled choices allow full merge via label-set union for uninvolved participants. Payload sends do NOT — a payload receive is an "opaque pipe" with no discriminating label. So:
```
p -> q {l1: q -> r {a: ...}, l2: q -> r {b: ...}}   -- projectable (r merges by label union)
p -> q {l1: q -> r [int]; ..., l2: q -> r [bool]; ...}  -- NOT projectable (r can't merge)
```

## File Structure

**Modified files:**
| File | Responsibility |
|------|----------------|
| `src/Syntax/AST.hs` | `PayloadType`, new constructors for `GlobalType`, `LocalType`, `Process`, `Expr` |
| `src/Syntax/Parser.hs` | Parse `[int]`, `[bool]`, `[unit]`, payload global/local/process syntax |
| `src/Syntax/Pretty.hs` | Render payload types and new constructors |
| `src/Syntax/WellFormed.hs` | Validate payload constructs (guardedness, self-comm) |
| `src/Automata.hs` | New node/edge types, build/reconstruct, context graph, hint completion |
| `src/Project.hs` | Handle `GlobalPayloadNode` in projection (coinductive + inductive) |
| `src/Merge.hs` | Handle payload nodes in iso/bisim/fullMerge |
| `src/Subtyping.hs` | Handle payload nodes in simulation |
| `src/Safety.hs` | Handle payload edges in safety checking |
| `src/Synthesise.hs` | Recognize/reconstruct payload edges |
| `src/MpstkBackend.hs` | Convert payload local types to mpstk format |
| `src/MPST.hs` | Re-export `PayloadType` |
| `test/ProjectionSpec.hs` | Payload projection tests |
| `test/SubtypingSpec.hs` | Payload subtyping tests |
| `test/SynthesiseSpec.hs` | Payload synthesis tests |
| `test/MergeSpec.hs` | Payload merge tests |
| `test/MpstkBackendSpec.hs` | Payload mpstk translation tests |
| `test/TestGenerators.hs` | QuickCheck generators for payload types |
| `test/Spec.hs` | Ensure new specs are wired in |

---

## Chunk 1: AST Foundation + Syntax

### Task 1: PayloadType and AST Extensions

**Files:**
- Modify: `src/Syntax/AST.hs`

- [ ] **Step 1: Add PayloadType and update GlobalType**

In `src/Syntax/AST.hs`, add the `PayloadType` data type after the `Branches` type alias (line 41), and add `GPayload` to `GlobalType`:

```haskell
-- | Payload types for value-passing messages.
data PayloadType = PTInt | PTBool | PTUnit
  deriving (Eq, Ord, Show, Generic)

instance NFData PayloadType
```

Add to `GlobalType` (after `GMessage`):
```haskell
  | GPayload Participant Participant PayloadType GlobalType -- ^ p -> q [t]; G
```

Update the module export list to include `PayloadType(..)`.

- [ ] **Step 2: Add LPayloadSend/LPayloadRecv to LocalType**

Add to `LocalType` (after `LRecv`):
```haskell
  | LPayloadSend Participant PayloadType LocalType -- ^ p ![t]; T
  | LPayloadRecv Participant PayloadType LocalType -- ^ p ?[t]; T
```

- [ ] **Step 3: Add EUnit to Expr and payload process constructors**

Add to `Expr` (after `ENot`):
```haskell
  | EUnit                        -- ^ Unit literal ()
```

Add to `Process` (after `PRecv`):
```haskell
  | PSendPayload Participant Expr Process     -- ^ p ![e] . P
  | PRecvPayload Participant String Process   -- ^ p ?(x) . P
```

- [ ] **Step 4: Update substituteVar for new LocalType constructors**

Add cases in the `go` function (after the `LRecv` case):
```haskell
        LPayloadSend peer pt cont ->
          LPayloadSend peer pt (go cont)
        LPayloadRecv peer pt cont ->
          LPayloadRecv peer pt (go cont)
```

- [ ] **Step 5: Update entryHintsLocal, entryHintsGlobal patterns**

In `unfoldRec`, no changes needed (the `_ -> localType` fallback handles new constructors).

- [ ] **Step 6: Verify compilation**

Run: `stack build --fast 2>&1 | head -50`

This will show compilation errors in downstream modules that pattern-match on GlobalType/LocalType/Process. That's expected — those are fixed in subsequent tasks.

- [ ] **Step 7: Commit**

```bash
git add src/Syntax/AST.hs
git commit -m "feat(ast): add PayloadType and payload constructors for GlobalType, LocalType, Process"
```

---

### Task 2: Parser

**Files:**
- Modify: `src/Syntax/Parser.hs`

- [ ] **Step 1: Add payloadTypeParser**

Add after the `typeVarP` parser:
```haskell
payloadTypeP :: Parser PayloadType
payloadTypeP = choice
  [ PTInt  <$ keyword "int"
  , PTBool <$ keyword "bool"
  , PTUnit <$ keyword "unit"
  ]
```

Add `"int"`, `"bool"`, `"unit"` to the reserved words list (the `keyword` check in `identifier`, around line 160). Currently `identifier` rejects keywords — add these three to whatever set is checked.

- [ ] **Step 2: Add global payload parser**

In `globalTypeParser`, add a `try gPayload` alternative before `try gMessage` in the `choice` list:

```haskell
    gPayload = do
      sender <- participantP
      _ <- symbol "->"
      receiver <- participantP
      pt <- between (symbol "[") (symbol "]") payloadTypeP
      _ <- symbol ";"
      cont <- globalTypeParser
      pure (GPayload sender receiver pt cont)
```

The `choice` list becomes:
```haskell
  choice [ gRec, GEnd <$ keyword "end", try gPayload, try gMessage, GVar <$> typeVarP, parens globalTypeParser ]
```

Note: `try gPayload` must come before `try gMessage` because both start with `participantP >> symbol "->" >> participantP`.

- [ ] **Step 3: Add local payload parsers**

In `localTypeParser`, add `try lPayloadSend` and `try lPayloadRecv` before the existing `try send` and `try recv`:

```haskell
    lPayloadSend = do
      peer <- participantP
      _ <- symbol "!"
      pt <- between (symbol "[") (symbol "]") payloadTypeP
      _ <- symbol ";"
      cont <- localTypeParser
      pure (LPayloadSend peer pt cont)

    lPayloadRecv = do
      peer <- participantP
      _ <- symbol "?"
      pt <- between (symbol "[") (symbol "]") payloadTypeP
      _ <- symbol ";"
      cont <- localTypeParser
      pure (LPayloadRecv peer pt cont)
```

- [ ] **Step 4: Add process payload parsers**

In `processParser`, add `try pSendPayload` before `try pSend`, and `try pRecvPayload` before `try pRecv`:

```haskell
    pSendPayload = do
      peer <- participantP
      _ <- symbol "!"
      e <- between (symbol "[") (symbol "]") exprParser
      _ <- symbol "."
      cont <- processParser
      pure (PSendPayload peer e cont)

    pRecvPayload = do
      peer <- participantP
      _ <- symbol "?"
      _ <- symbol "("
      var <- identifier
      _ <- symbol ")"
      _ <- symbol "."
      cont <- processParser
      pure (PRecvPayload peer var cont)
```

- [ ] **Step 5: Add EUnit to exprParser**

Add a `try` for unit literal `()` in `exprParser`, before other alternatives in the atom parser:
```haskell
    eUnit = EUnit <$ (symbol "(" *> symbol ")")
```

- [ ] **Step 6: Write parser tests**

Create tests in a convenient location (these can go in `test/ProjectionSpec.hs` or a new file, but simplest is to test via parse + render round-trips).

Test that these parse correctly:
```haskell
-- Global
parseGlobalType "p -> q [int]; end" == Right (GPayload (Participant "p") (Participant "q") PTInt GEnd)

-- Local
parseLocalType "q ! [bool]; end" == Right (LPayloadSend (Participant "q") PTBool LEnd)
parseLocalType "q ? [unit]; end" == Right (LPayloadRecv (Participant "q") PTUnit LEnd)

-- Process
parseProcess "q ! [42] . 0" == Right (PSendPayload (Participant "q") (EInt 42) PEnd)
parseProcess "q ? (x) . 0" == Right (PRecvPayload (Participant "q") "x" PEnd)
```

- [ ] **Step 7: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 8: Commit**

```bash
git add src/Syntax/Parser.hs
git commit -m "feat(parser): parse payload type syntax for global, local, and process types"
```

---

### Task 3: Pretty Printer

**Files:**
- Modify: `src/Syntax/Pretty.hs`

- [ ] **Step 1: Add prettyPayloadType**

```haskell
prettyPayloadType :: PayloadType -> Doc ann
prettyPayloadType PTInt  = pretty "int"
prettyPayloadType PTBool = pretty "bool"
prettyPayloadType PTUnit = pretty "unit"
```

- [ ] **Step 2: Add GlobalType payload case**

After the `GMessage` case in `prettyGlobalType`:
```haskell
prettyGlobalType (GPayload p q pt cont) =
  prettyParticipant p <+> pretty "->" <+> prettyParticipant q
    <+> brackets (prettyPayloadType pt) <> pretty ";"
    <+> prettyGlobalType cont
```

- [ ] **Step 3: Add LocalType payload cases**

After the `LRecv` case in `prettyLocalType`:
```haskell
prettyLocalType (LPayloadSend p pt cont) =
  prettyParticipant p <+> pretty "!" <> brackets (prettyPayloadType pt) <> pretty ";"
    <+> prettyLocalType cont
prettyLocalType (LPayloadRecv p pt cont) =
  prettyParticipant p <+> pretty "?" <> brackets (prettyPayloadType pt) <> pretty ";"
    <+> prettyLocalType cont
```

- [ ] **Step 4: Add Process payload cases and EUnit**

After the `PRecv` case in `prettyProcess`:
```haskell
prettyProcess (PSendPayload p e cont) =
  prettyParticipant p <+> pretty "!" <> brackets (prettyExpr e) <+> pretty "."
    <+> prettyProcess cont
prettyProcess (PRecvPayload p var cont) =
  prettyParticipant p <+> pretty "?" <> parens (pretty var) <+> pretty "."
    <+> prettyProcess cont
```

In `prettyExprPrec`:
```haskell
prettyExprPrec _ EUnit = pretty "()"
```

- [ ] **Step 5: Write parse-render round-trip tests**

Verify: `renderGlobalType (parse "p -> q [int]; end") == "p -> q [int]; end"` (modulo whitespace).

- [ ] **Step 6: Commit**

```bash
git add src/Syntax/Pretty.hs
git commit -m "feat(pretty): render payload types in global, local, and process types"
```

---

### Task 4: Well-Formedness

**Files:**
- Modify: `src/Syntax/WellFormed.hs`

- [ ] **Step 1: Add GPayload case in checkGlobal**

After the `GMessage` case (around line 75):
```haskell
      GPayload sender receiver _ body ->
        [ SelfCommunication sender | sender == receiver ]
        ++ checkGlobal (fmap (const True) env) body
```

- [ ] **Step 2: Add LPayloadSend/LPayloadRecv cases in checkLocal**

After the `LRecv` case (around line 99):
```haskell
      LPayloadSend _ _ cont ->
        checkLocal (fmap (const True) env) cont
      LPayloadRecv _ _ cont ->
        checkLocal (fmap (const True) env) cont
```

- [ ] **Step 3: Add PSendPayload/PRecvPayload cases in checkProc**

After the `PRecv` case (around line 150):
```haskell
      PSendPayload _ _ cont ->
        checkProc (fmap (const True) env) cont
      PRecvPayload _ _ cont ->
        checkProc (fmap (const True) env) cont
```

- [ ] **Step 4: Write well-formedness tests**

Test that `validateGlobalType (GPayload p p PTInt GEnd)` returns `SelfCommunication` error.
Test that `validateGlobalType (GPayload p q PTInt (GVar (TypeVar "t")))` returns `FreeTypeVar` error.
Test that `validateGlobalType (GRec (TypeVar "t") (GPayload p q PTInt (GVar (TypeVar "t"))))` succeeds (payload guards the recursion variable).

- [ ] **Step 5: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 6: Commit**

```bash
git add src/Syntax/WellFormed.hs
git commit -m "feat(wellformed): validate payload type constructs"
```

---

## Chunk 2: Automata Layer

### Task 5: Automata Types and Global Graph

**Files:**
- Modify: `src/Automata.hs`

This is the largest single task. It modifies the automata representation and all build/reconstruct functions.

- [ ] **Step 1: Add GlobalPayloadNode and GlobalPayloadEdgeLabel**

After `GlobalNode` definition (line 82):
```haskell
data GlobalNode
  = GlobalNode
  | GlobalPayloadNode    -- NEW: payload send node (single outgoing edge)
  | GlobalEndNode
  deriving (Eq, Show, Generic)
```

After `GlobalEdgeLabel` definition (line 125):
```haskell
data GlobalPayloadEdgeLabel = GlobalPayloadEdgeLabel
  { gpeSender :: Participant
  , gpeReceiver :: Participant
  , gpePayload :: PayloadType
  , gpeTargetHints :: RecVarHints
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData GlobalPayloadEdgeLabel
```

- [ ] **Step 2: Add ggPayloadEdges to GlobalGraph**

```haskell
data GlobalGraph = GlobalGraph
  { ggGraph :: G.Graph
  , ggStart :: G.Vertex
  , ggNodes :: G.Table GlobalNode
  , ggEdgeLabels :: Map.Map G.Edge [GlobalEdgeLabel]
  , ggPayloadEdges :: Map.Map G.Edge [GlobalPayloadEdgeLabel]  -- NEW
  , ggStartVarHints :: RecVarHints
  }
```

- [ ] **Step 3: Update globalNode builder to handle GPayload**

Change the type signature of `globalNode` to use `Either GlobalEdgeLabel GlobalPayloadEdgeLabel`:

```haskell
globalNode ::
  Env.Map TypeVar G.Vertex ->
  GlobalType ->
  State (GraphBuilder GlobalNode (Either GlobalEdgeLabel GlobalPayloadEdgeLabel)) G.Vertex
globalNode env gtype = case gtype of
  GMessage sender receiver branches -> do
    v <- freshNode GlobalNode
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- globalNode env cont
      addEdge v dest (Left (GlobalEdgeLabel sender receiver lbl (entryHintsGlobal cont)))
    pure v
  GPayload sender receiver pt cont -> do     -- NEW
    v <- freshNode GlobalPayloadNode
    dest <- globalNode env cont
    addEdge v dest (Right (GlobalPayloadEdgeLabel sender receiver pt (entryHintsGlobal cont)))
    pure v
  GVar var -> lookupVar env var
  GRec var body ->
    mfix $ \start -> globalNode (Env.insert var start env) body
  GEnd -> freshNode GlobalEndNode
```

- [ ] **Step 4: Update finaliseGlobal to partition edges**

```haskell
finaliseGlobal ::
  G.Vertex ->
  RecVarHints ->
  GraphBuilder GlobalNode (Either GlobalEdgeLabel GlobalPayloadEdgeLabel) ->
  GlobalGraph
finaliseGlobal start startHints builder =
  let bounds = graphBounds builder
      allEdges = gbEdges builder
      graph = G.buildG bounds (map fst allEdges)
      nodeTable = array bounds (Map.toList (gbNodes builder))
      branchEdges = [(e, lbl) | (e, Left lbl) <- allEdges]
      payloadEdges = [(e, plbl) | (e, Right plbl) <- allEdges]
   in GlobalGraph
        { ggGraph = graph
        , ggStart = start
        , ggNodes = nodeTable
        , ggEdgeLabels = collectEdges branchEdges
        , ggPayloadEdges = collectEdges payloadEdges
        , ggStartVarHints = normaliseRecVarHints startHints
        }
```

- [ ] **Step 5: Update entryHintsGlobal for GPayload**

Add case:
```haskell
        GPayload _ _ _ cont -> go (reverse binders) cont  -- no new binder, just pass through
```

Wait — `entryHintsGlobal` collects binders from nested `GRec`s. For `GPayload`, the hints come from the continuation. Actually, `GPayload` is not `GRec`, so the fallback `_ ->` case already handles it correctly (it returns the accumulated binders with no preferred var). No change needed.

- [ ] **Step 6: Update globalGraphToType to reconstruct GPayload**

In `buildAt`, after the `GlobalEndNode` case, add a case for `GlobalPayloadNode`:
```haskell
            GlobalPayloadNode -> buildPayloadNode (Set.insert v path) activeNames' v
```

Add the new helper:
```haskell
    buildPayloadNode ::
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      G.Vertex ->
      Either GraphToTypeError GlobalType
    buildPayloadNode path activeNames v =
      case Map.lookup v payloadOutgoing of
        Nothing ->
          Left (GraphToTypeInvalidGraph
            ("Payload vertex " ++ show v ++ " has no outgoing payload transition"))
        Just [(edgeLbl, dst)] -> do
          cont <- buildAt path activeNames (gpeTargetHints edgeLbl) dst
          pure (GPayload (gpeSender edgeLbl) (gpeReceiver edgeLbl) (gpePayload edgeLbl) cont)
        Just branches ->
          Left (GraphToTypeInvalidGraph
            ("Payload vertex " ++ show v ++ " has " ++ show (length branches) ++ " outgoing transitions, expected 1"))
```

Where `payloadOutgoing` is built similarly to `outgoing`:
```haskell
    payloadOutgoing = globalPayloadOutgoing completed

globalPayloadOutgoing :: GlobalGraph -> Map.Map G.Vertex [(GlobalPayloadEdgeLabel, G.Vertex)]
globalPayloadOutgoing gg =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty
    (ggPayloadEdges gg)
```

- [ ] **Step 7: Update hint completion for payload edges**

The `TransitionId` type needs to handle both branch and payload transitions. Change:
```haskell
data TransitionKey
  = TKBranch Label
  | TKPayload PayloadType
  deriving (Eq, Ord, Show)

type TransitionId = (G.Vertex, G.Vertex, TransitionKey)
```

Update `globalTransitions` to include payload transitions:
```haskell
globalTransitions :: GlobalGraph -> [(G.Vertex, G.Vertex, TransitionKey, RecVarHints)]
globalTransitions gg =
  [ (from, to, TKBranch (geLabel lbl), geTargetHints lbl)
  | ((from, to), labels) <- Map.toList (ggEdgeLabels gg)
  , lbl <- labels
  ]
  ++
  [ (from, to, TKPayload (gpePayload lbl), gpeTargetHints lbl)
  | ((from, to), labels) <- Map.toList (ggPayloadEdges gg)
  , lbl <- labels
  ]
```

Update `buildAdjacency`, `allHintNames`, `applyTransitionHints`, `completeGlobalHints` accordingly to use the new `TransitionId`. The logic is the same — just the key type changes from `Label` to `TransitionKey`.

For `applyTransitionHints`, it now needs to rewrite both edge maps:
```haskell
applyTransitionHints ::
  GlobalGraph ->
  Map.Map TransitionId RecVarHints ->
  GlobalGraph
applyTransitionHints gg hintsByTransition =
  gg
    { ggEdgeLabels = Map.mapWithKey rewriteBranch (ggEdgeLabels gg)
    , ggPayloadEdges = Map.mapWithKey rewritePayload (ggPayloadEdges gg)
    }
  where
    rewriteBranch (from, to) labels =
      fmap (\lbl -> lbl {geTargetHints = Map.findWithDefault (geTargetHints lbl) (from, to, TKBranch (geLabel lbl)) hintsByTransition}) labels
    rewritePayload (from, to) labels =
      fmap (\lbl -> lbl {gpeTargetHints = Map.findWithDefault (gpeTargetHints lbl) (from, to, TKPayload (gpePayload lbl)) hintsByTransition}) labels
```

- [ ] **Step 8: Update globalOutgoing (in Automata.hs)**

The existing `globalOutgoing` in Automata.hs (line 514) only handles branch edges. It is used by `globalGraphToType`. Keep it for branch edges; add `globalPayloadOutgoing` for payload edges (already done in step 6).

- [ ] **Step 9: Verify global graph build + reconstruct round-trips**

Write a test:
```haskell
let gt = GPayload (Participant "p") (Participant "q") PTInt GEnd
    gg = buildGlobalGraph gt
in globalGraphToType gg `shouldBe` Right gt
```

And a mixed test:
```haskell
let gt = GMessage (Participant "p") (Participant "q")
           ((Label "l", GPayload (Participant "q") (Participant "r") PTInt GEnd) :| [])
    gg = buildGlobalGraph gt
in globalGraphToType gg `shouldBe` Right gt
```

- [ ] **Step 10: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 11: Commit**

```bash
git add src/Automata.hs
git commit -m "feat(automata): add payload node/edge types and global graph build/reconstruct"
```

---

### Task 6: Local Graph and Context Graph

**Files:**
- Modify: `src/Automata.hs` (continued)

- [ ] **Step 1: Add LocalPayloadSendNode, LocalPayloadRecvNode, LocalPayloadEdgeLabel**

Update `LocalNode`:
```haskell
data LocalNode
  = LocalSendNode Participant [Label]
  | LocalRecvNode Participant [Label]
  | LocalPayloadSendNode Participant PayloadType  -- NEW
  | LocalPayloadRecvNode Participant PayloadType  -- NEW
  | LocalEndNode
  deriving (Eq, Show, Generic)
```

Add:
```haskell
data LocalPayloadEdgeLabel = LocalPayloadEdgeLabel
  { lpeDirection :: LocalDirection
  , lpePeer :: Participant
  , lpePayload :: PayloadType
  , lpeTargetHints :: RecVarHints
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LocalPayloadEdgeLabel
```

- [ ] **Step 2: Add lgPayloadEdges to LocalGraph**

```haskell
data LocalGraph = LocalGraph
  { lgGraph :: G.Graph
  , lgStart :: G.Vertex
  , lgNodes :: G.Table LocalNode
  , lgEdgeLabels :: Map.Map G.Edge [LocalEdgeLabel]
  , lgPayloadEdges :: Map.Map G.Edge [LocalPayloadEdgeLabel]  -- NEW
  , lgStartVarHints :: RecVarHints
  }
```

- [ ] **Step 3: Update localNode builder**

Change the type and add cases:
```haskell
localNode ::
  Env.Map TypeVar G.Vertex ->
  LocalType ->
  State (GraphBuilder LocalNode (Either LocalEdgeLabel LocalPayloadEdgeLabel)) G.Vertex
localNode env lt = case lt of
  LSend peer branches -> do
    let labels = fmap fst (NE.toList branches)
    v <- freshNode (LocalSendNode peer labels)
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- localNode env cont
      addEdge v dest (Left (LocalEdgeLabel Send peer lbl (entryHintsLocal cont)))
    pure v
  LRecv peer branches -> do
    let labels = fmap fst (NE.toList branches)
    v <- freshNode (LocalRecvNode peer labels)
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- localNode env cont
      addEdge v dest (Left (LocalEdgeLabel Receive peer lbl (entryHintsLocal cont)))
    pure v
  LPayloadSend peer pt cont -> do    -- NEW
    v <- freshNode (LocalPayloadSendNode peer pt)
    dest <- localNode env cont
    addEdge v dest (Right (LocalPayloadEdgeLabel Send peer pt (entryHintsLocal cont)))
    pure v
  LPayloadRecv peer pt cont -> do    -- NEW
    v <- freshNode (LocalPayloadRecvNode peer pt)
    dest <- localNode env cont
    addEdge v dest (Right (LocalPayloadEdgeLabel Receive peer pt (entryHintsLocal cont)))
    pure v
  LVar var -> lookupVar env var
  LRec var body ->
    mfix $ \start -> localNode (Env.insert var start env) body
  LEnd -> freshNode LocalEndNode
```

- [ ] **Step 4: Update finaliseLocal to partition edges**

Same pattern as `finaliseGlobal`:
```haskell
finaliseLocal ::
  G.Vertex ->
  RecVarHints ->
  GraphBuilder LocalNode (Either LocalEdgeLabel LocalPayloadEdgeLabel) ->
  LocalGraph
finaliseLocal start startHints builder =
  let bounds = graphBounds builder
      allEdges = gbEdges builder
      graph = G.buildG bounds (map fst allEdges)
      nodeTable = array bounds (Map.toList (gbNodes builder))
      branchEdges = [(e, lbl) | (e, Left lbl) <- allEdges]
      payloadEdges = [(e, plbl) | (e, Right plbl) <- allEdges]
   in LocalGraph
        { lgGraph = graph
        , lgStart = start
        , lgNodes = nodeTable
        , lgEdgeLabels = collectEdges branchEdges
        , lgPayloadEdges = collectEdges payloadEdges
        , lgStartVarHints = normaliseRecVarHints startHints
        }
```

- [ ] **Step 5: Update localGraphToType to reconstruct payload types**

In `buildAt`, add cases for `LocalPayloadSendNode` and `LocalPayloadRecvNode`:
```haskell
            LocalPayloadSendNode peer pt ->
              buildPayloadNode Send peer pt (Set.insert v path) activeNames' v
            LocalPayloadRecvNode peer pt ->
              buildPayloadNode Receive peer pt (Set.insert v path) activeNames' v
```

Add helper:
```haskell
    buildPayloadNode ::
      LocalDirection ->
      Participant ->
      PayloadType ->
      Set.Set G.Vertex ->
      Map.Map TypeVar G.Vertex ->
      G.Vertex ->
      Either GraphToTypeError LocalType
    buildPayloadNode dir peer pt path activeNames v =
      case Map.lookup v localPayloadOut of
        Nothing ->
          Left (GraphToTypeInvalidGraph
            ("Local payload vertex " ++ show v ++ " has no outgoing payload transition"))
        Just [(edgeLbl, dst)] -> do
          cont <- buildAt path activeNames (lpeTargetHints edgeLbl) dst
          pure $ case dir of
            Send -> LPayloadSend peer pt cont
            Receive -> LPayloadRecv peer pt cont
        Just branches ->
          Left (GraphToTypeInvalidGraph
            ("Local payload vertex " ++ show v ++ " has " ++ show (length branches) ++ " transitions, expected 1"))
```

Where `localPayloadOut` is:
```haskell
    localPayloadOut = localPayloadOutgoing completed

localPayloadOutgoing :: LocalGraph -> Map.Map G.Vertex [(LocalPayloadEdgeLabel, G.Vertex)]
localPayloadOutgoing lg =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty
    (lgPayloadEdges lg)
```

- [ ] **Step 6: Update entryHintsLocal for payload constructors**

Add cases in `entryHintsLocal`:
```haskell
        LPayloadSend _ _ cont -> go [] cont  -- not a binder, restart
        LPayloadRecv _ _ cont -> go [] cont
```

Actually, `entryHintsLocal` collects binders from leading `LRec`s. Payload constructors are not `LRec`, so the `_ ->` fallback handles them. But wait — we need to also restart binder collection. Looking at the current code:
```haskell
    go binders ltype = case ltype of
        LRec var body -> go (var : binders) body
        LVar var -> ... (reverse binders, Just var)
        _ -> ... (reverse binders, Nothing)
```
The `_ ->` case returns accumulated binders with no preferred var. This is correct for payload nodes — they're like `LSend`/`LRecv` in that they don't add binders. No change needed.

- [ ] **Step 7: Update local hint completion**

Same pattern as global: change `LocalTransitionId` to handle both kinds:
```haskell
type LocalTransitionId = (G.Vertex, G.Vertex, LocalDirection, Participant, Either Label PayloadType)
```

Update `localTransitionId`:
```haskell
localTransitionId :: G.Vertex -> G.Vertex -> LocalEdgeLabel -> LocalTransitionId
localTransitionId from to lbl =
  (from, to, leDirection lbl, lePeer lbl, Left (leLabel lbl))

localPayloadTransitionId :: G.Vertex -> G.Vertex -> LocalPayloadEdgeLabel -> LocalTransitionId
localPayloadTransitionId from to lbl =
  (from, to, lpeDirection lbl, lpePeer lbl, Right (lpePayload lbl))
```

Update `localTransitions`, `buildLocalAdjacency`, `allLocalHintNames`, `applyLocalTransitionHints`, `completeLocalHints` to include payload transitions. The pattern mirrors the global hint completion changes.

- [ ] **Step 8: Add payload context edge constructors**

Add to `ContextEdgeLabel`:
```haskell
  | ContextPayloadSingleEdge
      { ceActor :: Participant
      , ceDirection :: LocalDirection
      , cePeer :: Participant
      , cePayloadType :: PayloadType
      }
  | ContextPayloadSyncEdge
      { ceSender :: Participant
      , ceReceiver :: Participant
      , cePayloadType :: PayloadType
      }
```

- [ ] **Step 9: Update contextTransitions for payload edges**

In `contextTransitions`, the `singleTransitions` list comprehension iterates `LocalStep`s which contain `LocalEdgeLabel`s. Payload edges use `LocalPayloadEdgeLabel`. We need to also collect payload steps from the components.

Update `LocalStep` to handle both:
```haskell
data LocalStep
  = BranchStep
      { lsTo :: !G.Vertex
      , lsLabel :: LocalEdgeLabel
      }
  | PayloadStep
      { lsTo :: !G.Vertex
      , lsPayloadLabel :: LocalPayloadEdgeLabel
      }
```

Update `outgoingSteps` to produce both kinds from both edge maps:
```haskell
outgoingSteps :: LocalGraph -> Map.Map G.Vertex [LocalStep]
outgoingSteps lg =
  foldl' addBranch (foldl' addPayload Map.empty (Map.toList (lgPayloadEdges lg))) (Map.toList (lgEdgeLabels lg))
  where
    addBranch acc ((from, to), labels) =
      foldl' (\m lbl -> Map.insertWith (++) from [BranchStep to lbl] m) acc labels
    addPayload acc ((from, to), labels) =
      foldl' (\m lbl -> Map.insertWith (++) from [PayloadStep to lbl] m) acc labels
```

Update `singleTransitions` and `syncTransitions` to handle both step kinds:
```haskell
    singleTransitions =
      [ case step of
          BranchStep to lbl ->
            ( ContextState (Map.insert participant to states)
            , ContextSingleEdge participant (leDirection lbl) (lePeer lbl) (leLabel lbl)
            )
          PayloadStep to lbl ->
            ( ContextState (Map.insert participant to states)
            , ContextPayloadSingleEdge participant (lpeDirection lbl) (lpePeer lbl) (lpePayload lbl)
            )
      | (participant, comp) <- Map.toList components
      , step <- outgoingAt participant comp
      ]

    syncTransitions = branchSyncs ++ payloadSyncs

    branchSyncs =
      [ ... existing code filtering for BranchStep ... ]

    payloadSyncs =
      [ ( ContextState
            (Map.insert receiver (lsTo recvStep) (Map.insert sender (lsTo sendStep) states))
        , ContextPayloadSyncEdge sender receiver (lpePayload sendLbl)
        )
      | (sender, senderComp) <- Map.toList components
      , PayloadStep sendTo sendLbl <- outgoingAt sender senderComp
      , lpeDirection sendLbl == Send
      , let receiver = lpePeer sendLbl
            sendStep = PayloadStep sendTo sendLbl
      , Just receiverComp <- [Map.lookup receiver components]
      , PayloadStep recvTo recvLbl <- outgoingAt receiver receiverComp
      , let recvStep = PayloadStep recvTo recvLbl
      , lpeDirection recvLbl == Receive
      , lpePeer recvLbl == sender
      , lpePayload recvLbl == lpePayload sendLbl
      ]
```

- [ ] **Step 10: Update checkContextSynchrony**

Add payload edges to the send/recv/sync key checks. The `checkSource` function needs to consider `ContextPayloadSingleEdge` and `ContextPayloadSyncEdge`:

Use a unified key type or separate checks. Simplest: add a `ContextInvariantError` variant for payload edges, then check payload synchrony similarly to branch synchrony.

- [ ] **Step 11: Update module export list**

Export: `GlobalPayloadNode` (part of `GlobalNode(..)`), `GlobalPayloadEdgeLabel(..)`, `LocalPayloadSendNode`, `LocalPayloadRecvNode` (part of `LocalNode(..)`), `LocalPayloadEdgeLabel(..)`, `ContextPayloadSingleEdge`, `ContextPayloadSyncEdge` (part of `ContextEdgeLabel(..)`), `globalPayloadOutgoing`, `localPayloadOutgoing`.

- [ ] **Step 12: Verify local graph round-trips**

Test:
```haskell
let lt = LPayloadSend (Participant "q") PTInt LEnd
    lg = buildLocalGraph lt
in localGraphToType lg `shouldBe` Right lt
```

- [ ] **Step 13: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 14: Commit**

```bash
git add src/Automata.hs
git commit -m "feat(automata): add local payload graph building, context payload edges"
```

---

## Chunk 3: Projection and Merge

### Task 7: Projection

**Files:**
- Modify: `src/Project.hs`

- [ ] **Step 1: Update imports**

Add to import list from Automata:
```haskell
  , GlobalPayloadEdgeLabel(..)
  , LocalPayloadEdgeLabel(..)
  , globalPayloadOutgoing
```

Add to import from Syntax.AST:
```haskell
  , PayloadType
```

- [ ] **Step 2: Update ProjEnv to include payload outgoing**

```haskell
data ProjEnv = ProjEnv
  { peParticipant :: Participant
  , peAllowRecvUnion :: Bool
  , peDeferMerges :: Bool
  , peMerge :: Merge
  , peGlobalNodes :: Map.Map G.Vertex GlobalNode
  , peOutgoing :: Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)]
  , pePayloadOutgoing :: Map.Map G.Vertex [(GlobalPayloadEdgeLabel, G.Vertex)]  -- NEW
  }
```

Update `projectInductiveWith` to populate it:
```haskell
      , pePayloadOutgoing = globalPayloadOutgoing gg
```

- [ ] **Step 3: Handle GlobalPayloadNode in projectAt**

Add case after `GlobalEndNode` and `GlobalNode`:
```haskell
        GlobalPayloadNode -> do
          payloadEdge <- liftEither $ payloadOutgoingAt gv (pePayloadOutgoing env)
          projectPayloadNode env hints gv lv payloadEdge
```

- [ ] **Step 4: Implement projectPayloadNode**

```haskell
projectPayloadNode ::
  ProjEnv ->
  RecVarHints ->
  G.Vertex ->
  G.Vertex ->
  (GlobalPayloadEdgeLabel, G.Vertex) ->
  ProjM ProjectionTarget
projectPayloadNode env hints gv lv (edgeLbl, child) = do
  let sender = gpeSender edgeLbl
      receiver = gpeReceiver edgeLbl
      pt = gpePayload edgeLbl
      p = peParticipant env
  if p == sender then do
    -- Sender: emit payload send node
    ensureNodeM lv (LocalPayloadSendNode receiver pt)
    childStart <- freshLocalVertex
    target <- projectAt env Set.empty (gpeTargetHints edgeLbl) child childStart
    let edge = Right (LocalPayloadEdgeLabel Send receiver pt (ptHints target))
    insertPayloadEdgeM lv (ptVertex target) edge
    pure (ProjectionTarget lv hints)
  else if p == receiver then do
    -- Receiver: emit payload recv node
    ensureNodeM lv (LocalPayloadRecvNode sender pt)
    childStart <- freshLocalVertex
    target <- projectAt env Set.empty (gpeTargetHints edgeLbl) child childStart
    let edge = Right (LocalPayloadEdgeLabel Receive sender pt (ptHints target))
    insertPayloadEdgeM lv (ptVertex target) edge
    pure (ProjectionTarget lv hints)
  else
    -- Uninvolved: just project the continuation at the same local vertex
    projectAt env Set.empty (appendHints hints (gpeTargetHints edgeLbl)) child lv
```

Note: For uninvolved participants, payload projection simply follows the continuation (no merge needed, since there's only one branch).

- [ ] **Step 5: Update BuildGraph for payload edges**

```haskell
data BuildGraph = BuildGraph
  { bgNodes :: Map.Map G.Vertex LocalNode
  , bgEdges :: [((G.Vertex, G.Vertex), LocalEdgeLabel)]
  , bgPayloadEdges :: [((G.Vertex, G.Vertex), LocalPayloadEdgeLabel)]  -- NEW
  }

emptyBuildGraph :: BuildGraph
emptyBuildGraph = BuildGraph Map.empty [] []
```

Add `insertPayloadEdgeM`:
```haskell
insertPayloadEdgeM :: G.Vertex -> G.Vertex -> Either LocalEdgeLabel LocalPayloadEdgeLabel -> ProjM ()
insertPayloadEdgeM from to (Left lbl) = insertEdgeM from to lbl
insertPayloadEdgeM from to (Right plbl) = do
  build <- gets psBuild
  modify (\s -> s {psBuild = build {bgPayloadEdges = ((from, to), plbl) : bgPayloadEdges build}})
```

- [ ] **Step 6: Update materialiseLocalGraph for payload edges**

In `materialiseLocalGraph`, include payload edges in the graph and output:
```haskell
      payloadEdges =
        [ ((renaming Map.! from, renaming Map.! to), lbl)
        | ((from, to), lbl) <- bgPayloadEdges build
        , from `Map.member` renaming
        , to `Map.member` renaming
        ]
```

And in the output:
```haskell
      , lgPayloadEdges = collectEdges payloadEdges
```

Also include payload edges in the adjacency graph and BFS reachability.

Update `bfsReachable` to consider payload edges too:
```haskell
bfsReachable :: Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)] -> Map.Map G.Vertex [(LocalPayloadEdgeLabel, G.Vertex)] -> G.Vertex -> [G.Vertex]
bfsReachable out payloadOut start = go Set.empty [start] []
  where
    go _ [] acc = reverse acc
    go seen (v : vs) acc
      | v `Set.member` seen = go seen vs acc
      | otherwise =
          let branchSuccs = fmap snd (Map.findWithDefault [] v out)
              payloadSuccs = fmap snd (Map.findWithDefault [] v payloadOut)
           in go (Set.insert v seen) (vs ++ branchSuccs ++ payloadSuccs) (v : acc)
```

- [ ] **Step 7: Update coinductive projection for payload nodes**

In `buildCoindEnv`, add handling for `GlobalPayloadNode`:
```haskell
data CoindVertexInfo
  = CoindEnd
  | CoindMessage Participant Participant (Map.Map Label (GlobalEdgeLabel, G.Vertex))
  | CoindPayload Participant Participant PayloadType (GlobalPayloadEdgeLabel, G.Vertex)  -- NEW
```

Update `buildVertexInfo`:
```haskell
    GlobalPayloadNode -> do
      case Map.lookup v payloadOut of
        Nothing -> Left (ProjectionError ...)
        Just [(plbl, dst)] -> pure (CoindPayload (gpeSender plbl) (gpeReceiver plbl) (gpePayload plbl) (plbl, dst))
        Just _ -> Left (ProjectionError "Payload node has multiple outgoing edges")
```

Update `closureFor` to handle `CoindPayload` (uninvolved participants follow the continuation):
```haskell
        CoindPayload sender receiver _ (_, dst)
          | ceParticipant env == sender || ceParticipant env == receiver ->
              go closed pending
          | otherwise ->
              let (closed', pending') = addAll [dst] closed pending
               in go closed' pending'
```

Update `analyseClosedState` and `exploreCoind` similarly — `CoindPayload` for involved participants creates a payload send/recv node instead of a choice node.

- [ ] **Step 8: Add payloadOutgoingAt helper**

```haskell
payloadOutgoingAt :: G.Vertex -> Map.Map G.Vertex [(GlobalPayloadEdgeLabel, G.Vertex)] -> Either ProjectionError (GlobalPayloadEdgeLabel, G.Vertex)
payloadOutgoingAt v out =
  case Map.lookup v out of
    Just [(plbl, dst)] -> Right (plbl, dst)
    Just _ -> Left (ProjectionError ("Payload vertex " ++ show v ++ " has multiple outgoing transitions."))
    Nothing -> Left (ProjectionError ("Invalid global graph: payload vertex " ++ show v ++ " has no outgoing transitions."))
```

- [ ] **Step 9: Write projection tests**

Test cases:
1. **Involved projection**: `p -> q [int]; end` projected onto `p` gives `q ![int]; end`, onto `q` gives `p ?[int]; end`
2. **Uninvolved projection**: `p -> q [int]; end` projected onto `r` gives `end`
3. **Mixed**: `p -> q {l1: q -> r [int]; end, l2: q -> r [int]; end}` projected onto `r` — same payload type → merge succeeds (both branches give `q ?[int]; end`)
4. **Unprojectable**: `p -> q {l1: q -> r [int]; end, l2: q -> r [bool]; end}` projected onto `r` — different payload types → merge fails

```haskell
it "projects payload onto sender" $
  expectProjectionAs projectInductiveFull
    "p -> q [int]; end"
    "p"
    "q ! [int]; end"

it "projects payload onto receiver" $
  expectProjectionAs projectInductiveFull
    "p -> q [int]; end"
    "q"
    "p ? [int]; end"

it "projects payload onto uninvolved" $
  expectProjectionAs projectInductiveFull
    "p -> q [int]; end"
    "r"
    "end"

it "fails projection when payload types differ across branches" $
  expectProjectionFails projectInductiveFull
    "p -> q {l1: q -> r [int]; end, l2: q -> r [bool]; end}"
    "r"
```

- [ ] **Step 10: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 11: Commit**

```bash
git add src/Project.hs test/ProjectionSpec.hs
git commit -m "feat(projection): handle payload nodes in coinductive and inductive projection"
```

---

### Task 8: Merge

**Files:**
- Modify: `src/Merge.hs`

- [ ] **Step 1: Update imports**

Add `LocalPayloadEdgeLabel(..)`, `LocalPayloadSendNode`, `LocalPayloadRecvNode`, `PayloadType` to imports.

- [ ] **Step 2: Update nodeCompatible**

```haskell
nodeCompatible :: LocalNode -> LocalNode -> Bool
nodeCompatible left right =
  case (left, right) of
    (LocalEndNode, LocalEndNode) -> True
    (LocalSendNode p _, LocalSendNode q _) -> p == q
    (LocalRecvNode p _, LocalRecvNode q _) -> p == q
    (LocalPayloadSendNode p pt, LocalPayloadSendNode q qt) -> p == q && pt == qt  -- NEW
    (LocalPayloadRecvNode p pt, LocalPayloadRecvNode q qt) -> p == q && pt == qt  -- NEW
    _ -> False
```

- [ ] **Step 3: Update iso for payload edges**

The `iso` function uses `successorsByLabel` to get `Map.Map Label G.Vertex`. For payload nodes, there are no labels — just one successor. Update to handle both:

```haskell
    go fwd bwd ((x, y) : pending) =
      case (Map.lookup x fwd, Map.lookup y bwd) of
        (Just y', Just x') -> y' == y && x' == x && go fwd bwd pending
        (Nothing, Nothing) ->
          case (Map.lookup x leftNodes, Map.lookup y rightNodes) of
            (Just nx, Just ny) ->
              nodeCompatible nx ny
                && case (nx, ny) of
                  (LocalPayloadSendNode{}, LocalPayloadSendNode{}) ->
                    matchPayloadSuccessors leftPayloadOut rightPayloadOut fwd bwd x y pending
                  (LocalPayloadRecvNode{}, LocalPayloadRecvNode{}) ->
                    matchPayloadSuccessors leftPayloadOut rightPayloadOut fwd bwd x y pending
                  _ -> -- existing branch logic
                    case (successorsByLabel leftOut x, successorsByLabel rightOut y) of
                      ...
            _ -> False
        _ -> False
```

Where `matchPayloadSuccessors` checks that both have exactly one payload successor and recurses.

- [ ] **Step 4: Update bisim for payload edges**

Extend `pairCompatible` and `successorsStayIn` to consider payload outgoing maps. For payload nodes, the "label" is the payload type — use a similar keying scheme.

- [ ] **Step 5: Update fullMerge for payload edges**

In `expandAlign` > `mergeBoth`, add cases for payload nodes:
```haskell
    (LocalPayloadSendNode leftPeer leftPt, LocalPayloadSendNode rightPeer rightPt)
      | leftPeer == rightPeer && leftPt == rightPt ->
          -- Same peer and payload type: merge continuations
          mergePayloadBoth leftPeer leftPt Send ...
    (LocalPayloadRecvNode leftPeer leftPt, LocalPayloadRecvNode rightPeer rightPt)
      | leftPeer == rightPeer && leftPt == rightPt ->
          -- Same peer and payload type: merge continuations
          mergePayloadBoth leftPeer leftPt Receive ...
```

Payload merge: succeeds only if same peer AND same payload type. The single continuation is merged recursively.

In `cloneLeft`/`cloneRight`, add cases for payload nodes.

In `validateOutgoing`, add cases for payload nodes (they should have exactly one payload outgoing edge).

**Critical**: The `outgoingByLabelMap` function returns `Map.Map G.Vertex (Map.Map Label ...)`. For payload nodes, there are no labels. We need a parallel `outgoingByPayloadMap` or unified approach. Since fullMerge's MergeInput uses `Map.Map Label (LocalEdgeLabel, G.Vertex)`, we need to add payload edge data:

```haskell
data MergeInput = MergeInput
  { miLeftNodes :: Map.Map G.Vertex LocalNode
  , miRightNodes :: Map.Map G.Vertex LocalNode
  , miLeftOut :: Map.Map G.Vertex (Map.Map Label (LocalEdgeLabel, G.Vertex))
  , miRightOut :: Map.Map G.Vertex (Map.Map Label (LocalEdgeLabel, G.Vertex))
  , miLeftPayloadOut :: Map.Map G.Vertex (LocalPayloadEdgeLabel, G.Vertex)   -- NEW
  , miRightPayloadOut :: Map.Map G.Vertex (LocalPayloadEdgeLabel, G.Vertex)  -- NEW
  }
```

- [ ] **Step 6: Update buildMergedGraph for payload edges**

The `MergeBuild` state needs to track payload edges too:
```haskell
data MergeBuild = MergeBuild
  { ...existing fields...
  , mbPayloadEdges :: Set.Set (G.Edge, LocalPayloadEdgeLabel)  -- NEW
  }
```

And `buildMergedGraph` includes them:
```haskell
      , lgPayloadEdges = collectEdges (Set.toList (mbPayloadEdges st))
```

- [ ] **Step 7: Write merge tests**

```haskell
it "plain merge succeeds for identical payload types" $
  expectPlainMergeAs "q ! [int]; end" "q ! [int]; end" "q ! [int]; end"

it "plain merge fails for different payload types" $
  expectPlainMergeFails "q ! [int]; end" "q ! [bool]; end"

it "full merge fails for different payload receive types" $
  expectFullMergeFails "q ? [int]; end" "q ? [bool]; end"

it "full merge succeeds for same payload receive types" $
  expectFullMergeAs "q ? [int]; end" "q ? [int]; end" "q ? [int]; end"
```

- [ ] **Step 8: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 9: Commit**

```bash
git add src/Merge.hs test/MergeSpec.hs
git commit -m "feat(merge): handle payload nodes in iso, bisim, and full merge"
```

---

## Chunk 4: Analysis Algorithms

### Task 9: Subtyping

**Files:**
- Modify: `src/Subtyping.hs`

- [ ] **Step 1: Update imports**

Add `LocalPayloadEdgeLabel(..)`, `PayloadType` to imports.

- [ ] **Step 2: Update compatibleNodeKinds**

```haskell
    (LocalPayloadSendNode p pt, LocalPayloadSendNode q qt) -> p == q && pt == qt
    (LocalPayloadRecvNode p pt, LocalPayloadRecvNode q qt) -> p == q && pt == qt
```

- [ ] **Step 3: Update LocalTransition to handle payloads**

The current `LocalTransition` has `ltLabel :: Label`. For payload transitions, we need a different discriminator. Options:

a) Add a new `LocalPayloadTransition` type alongside
b) Use `Either Label PayloadType` in the transition

Go with (b) for simplicity:
```haskell
data TransitionContent = TCLabel Label | TCPayload PayloadType
  deriving (Eq, Ord, Show)

data LocalTransition = LocalTransition
  { ltTo :: !G.Vertex
  , ltDirection :: LocalDirection
  , ltPeer :: Participant
  , ltContent :: TransitionContent
  }
```

- [ ] **Step 4: Update collectOutgoing**

Include payload edges:
```haskell
collectOutgoing ::
  Map.Map G.Edge [LocalEdgeLabel] ->
  Map.Map G.Edge [LocalPayloadEdgeLabel] ->
  Map.Map G.Vertex [LocalTransition]
collectOutgoing branchEdges payloadEdges =
  foldPayloads (foldBranches Map.empty)
  where
    foldBranches = Map.foldlWithKey' (\acc (from, to) labels -> foldl' (addBranch from to) acc labels)
    foldPayloads = Map.foldlWithKey' (\acc (from, to) labels -> foldl' (addPayload from to) acc labels) ... payloadEdges
    addBranch from to acc label =
      Map.insertWith (++) from
        [LocalTransition to (leDirection label) (lePeer label) (TCLabel (leLabel label))]
        acc
    addPayload from to acc label =
      Map.insertWith (++) from
        [LocalTransition to (lpeDirection label) (lpePeer label) (TCPayload (lpePayload label))]
        acc
```

Update callers (`checkLocalSubtype`) to pass both edge maps.

- [ ] **Step 5: Update sameAction**

```haskell
sameAction lhs rhs =
  ltDirection lhs == ltDirection rhs
    && ltPeer lhs == ltPeer rhs
    && ltContent lhs == ltContent rhs
```

- [ ] **Step 6: Update violatesSimulation for payload nodes**

Add cases:
```haskell
    (Just (LocalPayloadSendNode _ _), Just (LocalPayloadSendNode _ _)) ->
      not (allLeftMatchedByRight leftSends rightSends)
    (Just (LocalPayloadRecvNode _ _), Just (LocalPayloadRecvNode _ _)) ->
      not (allRightMatchedByLeft rightRecvs leftRecvs)
```

Where `leftSends`/`rightSends` now include payload transitions filtered by direction.

- [ ] **Step 7: Write subtyping tests**

```haskell
it "payload send subtype: same type" $
  expectSubtype "q ! [int]; end" "q ! [int]; end"

it "payload send not subtype: different types" $
  expectNotSubtype "q ! [int]; end" "q ! [bool]; end"

it "payload recv subtype: same type" $
  expectSubtype "q ? [int]; end" "q ? [int]; end"
```

- [ ] **Step 8: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 9: Commit**

```bash
git add src/Subtyping.hs test/SubtypingSpec.hs
git commit -m "feat(subtyping): handle payload nodes in simulation-based subtyping"
```

---

### Task 10: Safety

**Files:**
- Modify: `src/Safety.hs`

- [ ] **Step 1: Update enabledSends and enabledReceives for payload edges**

Currently these functions pattern-match on `ContextSingleEdge actor Send/Receive peer label`. Add payload cases.

For safety, the key question: how do payload sends/receives interact with the safety condition?

The safety invariant says: if both send and receive are enabled for a pair (p,q), every enabled send label must also be an enabled receive label. For payload edges, there are no labels — the "content" is a payload type.

Extend the safety check: if both payload-send and payload-receive are enabled for (p,q), the payload types must match.

Create separate maps or use a unified key:
```haskell
data MessageContent = MCLabel Label | MCPayload PayloadType
  deriving (Eq, Ord, Show)
```

Update `enabledSends`/`enabledReceives` to return `Map.Map (Participant, Participant) (Set.Set MessageContent)`:
```haskell
    step acc (_, ContextSingleEdge actor Send peer label) =
      Map.insertWith Set.union (actor, peer) (Set.singleton (MCLabel label)) acc
    step acc (_, ContextPayloadSingleEdge actor Send peer pt) =
      Map.insertWith Set.union (actor, peer) (Set.singleton (MCPayload pt)) acc
    step acc _ = acc
```

Update `SafetyError` to use `MessageContent` instead of `Label`:
```haskell
data SafetyError = MissingReceiveLabel
  { seVertex :: !G.Vertex
  , seSender :: Participant
  , seReceiver :: Participant
  , seContent :: MessageContent       -- was: seLabel :: Label
  , seEnabledReceives :: Set.Set MessageContent  -- was: Set.Set Label
  }
```

Note: This changes the `SafetyError` type. If other modules depend on the field name `seLabel`, they need updating.

- [ ] **Step 2: Update sync reachability for payload sync edges**

In `collectSyncAdjacency`, add:
```haskell
    isSync ContextPayloadSyncEdge{} = True
```

- [ ] **Step 3: Write safety tests**

```haskell
it "safe context with payload" $ do
  -- p sends int to q, q receives int from p
  let ctx = [("p", "q ! [int]; end"), ("q", "p ? [int]; end")]
  expectSafe ctx

it "unsafe context with mismatched payload" $ do
  -- needs a context where send/receive payload types differ
  ...
```

- [ ] **Step 4: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 5: Commit**

```bash
git add src/Safety.hs
git commit -m "feat(safety): handle payload edges in safety checking"
```

---

### Task 11: Synthesis

**Files:**
- Modify: `src/Synthesise.hs`

- [ ] **Step 1: Update imports**

Add `GlobalPayloadEdgeLabel(..)`, `ContextPayloadSyncEdge`, `PayloadType` to imports.

- [ ] **Step 2: Update SynthState for payload edges**

```haskell
data SynthState = SynthState
  { ssNextVertex :: !G.Vertex
  , ssNodes :: Map.Map G.Vertex GlobalNode
  , ssEdges :: [((G.Vertex, G.Vertex), GlobalEdgeLabel)]
  , ssPayloadEdges :: [((G.Vertex, G.Vertex), GlobalPayloadEdgeLabel)]  -- NEW
  , ssEnv :: Map.Map (Participant, G.Vertex) G.Vertex
  }
```

- [ ] **Step 3: Update findSendActive for payload sync edges**

The current code collects senders from `ContextSyncEdge`. Also include `ContextPayloadSyncEdge`:
```haskell
      senders = S.fromList
        [ s
        | (_, e) <- edges
        , s <- case e of
            ContextSyncEdge{ceSender = s} -> [s]
            ContextPayloadSyncEdge{ceSender = s} -> [s]
            _ -> []
        ]
```

- [ ] **Step 4: Update synthNode to handle payload sync edges**

In `synthNode`, after collecting `syncEdges`, also collect payload sync edges:
```haskell
              payloadSyncEdges =
                [ (to, lbl)
                | (to, lbl@ContextPayloadSyncEdge{}) <- edges
                , ceSender lbl == sender
                ]
```

If `syncEdges` is non-empty, proceed with branch synthesis (existing logic). If `payloadSyncEdges` is non-empty, proceed with payload synthesis. If both are non-empty... this is an error (a sender can't do both at the same vertex). Add a check for this.

For payload synthesis:
```haskell
          -- Payload edges: should be exactly one
          case payloadSyncEdges of
            [(to, lbl)] -> do
              let receiver = ceReceiver lbl
                  pt = cePayloadType lbl
                  n = length participants
                  nextPriority = (senderIdx + 1) `mod` n
              (targetGlobalV, st3) <- synthNode cg participants outgoing to nextPriority st2
              let globalPayloadLbl = GlobalPayloadEdgeLabel sender receiver pt emptyRecVarHints
                  st4 = st3 { ssPayloadEdges = ((gNode, targetGlobalV), globalPayloadLbl) : ssPayloadEdges st3 }
              Right (gNode, st4)
            _ -> Left (InternalError "Multiple payload sync edges for same sender at same vertex")
```

Use `GlobalPayloadNode` for the fresh vertex in this case.

- [ ] **Step 5: Update finaliseGlobalGraph**

Include payload edges:
```haskell
      payloadEdgeLabels = collectEdges (ssPayloadEdges st)
   in GlobalGraph
        { ...
        , ggPayloadEdges = payloadEdgeLabels
        ...
        }
```

And include payload edges in the graph adjacency:
```haskell
      graph = G.buildG bounds (map fst (ssEdges st) ++ map fst (ssPayloadEdges st))
```

- [ ] **Step 6: Write synthesis tests**

```haskell
it "synthesises payload send/receive" $
  expectSynthGlobal
    [("p", "q ! [int]; end"), ("q", "p ? [int]; end")]
    "p -> q [int]; end"
```

- [ ] **Step 7: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 8: Commit**

```bash
git add src/Synthesise.hs test/SynthesiseSpec.hs
git commit -m "feat(synthesis): handle payload sync edges in global graph synthesis"
```

---

## Chunk 5: Backend and Integration

### Task 12: MpstkBackend

**Files:**
- Modify: `src/MpstkBackend.hs`

- [ ] **Step 1: Add payload cases to toMpstkLocalType**

```haskell
toMpstkLocalType (LPayloadSend (Participant p) pt cont) =
  p ++ " (+) {_payload_" ++ payloadTypeStr pt ++ ". " ++ toMpstkLocalType cont ++ "}"
toMpstkLocalType (LPayloadRecv (Participant p) pt cont) =
  p ++ " & {_payload_" ++ payloadTypeStr pt ++ ". " ++ toMpstkLocalType cont ++ "}"

payloadTypeStr :: PayloadType -> String
payloadTypeStr PTInt  = "int"
payloadTypeStr PTBool = "bool"
payloadTypeStr PTUnit = "unit"
```

Note: MPSTK doesn't natively support payload types. We encode them as a single-branch labeled choice with a synthetic label like `_payload_int`. This is the closest approximation.

- [ ] **Step 2: Write translation tests**

```haskell
it "translates payload send" $
  expectTranslation "q ! [int]; end" "q (+) {_payload_int. end}"

it "translates payload recv" $
  expectTranslation "q ? [bool]; end" "q & {_payload_bool. end}"
```

- [ ] **Step 3: Run tests**

Run: `stack test 2>&1 | tail -30`

- [ ] **Step 4: Commit**

```bash
git add src/MpstkBackend.hs test/MpstkBackendSpec.hs
git commit -m "feat(mpstk): translate payload types to mpstk format"
```

---

### Task 13: MPST Module and Test Generators

**Files:**
- Modify: `src/MPST.hs`
- Modify: `test/TestGenerators.hs`

- [ ] **Step 1: Re-export PayloadType from MPST.hs**

Add `PayloadType(..)` to the re-exports from `Syntax.AST`. Also re-export new automata types.

- [ ] **Step 2: Update TestGenerators for payload types**

Add payload alternatives to `genGlobal`:
```haskell
    genPayload = do
      sender <- genParticipant
      receiver <- genParticipant `suchThat` (/= sender)
      pt <- elements [PTInt, PTBool, PTUnit]
      size' <- choose (0, size - 1)
      cont <- genGlobal env size'
      pure (GPayload sender receiver pt cont)
```

Add to the `frequency` list.

Similarly for `genLocal` (add `LPayloadSend`/`LPayloadRecv` alternatives) and `genProc` (add `PSendPayload`/`PRecvPayload`).

Update `canonicalProcess`:
```haskell
    go (LPayloadSend p pt cont) = PSendPayload p (defaultExprForType pt) (go cont)
    go (LPayloadRecv p _ cont) = PRecvPayload p "_x" (go cont)

defaultExprForType :: PayloadType -> Expr
defaultExprForType PTInt  = EInt 0
defaultExprForType PTBool = EBool False
defaultExprForType PTUnit = EUnit
```

Update `labelsOfLocalType`:
```haskell
    labelsOfLocalType (LPayloadSend _ _ cont) = labelsOfLocalType cont
    labelsOfLocalType (LPayloadRecv _ _ cont) = labelsOfLocalType cont
```

Update `genContextLocal` to include payload options.

- [ ] **Step 3: Run full test suite**

Run: `stack test 2>&1 | tail -50`

- [ ] **Step 4: Commit**

```bash
git add src/MPST.hs test/TestGenerators.hs
git commit -m "feat(integration): re-export payload types, update QuickCheck generators"
```

---

### Task 14: Fix Remaining Compilation Errors and Exhaustiveness Warnings

**Files:**
- Any files with pattern match warnings

- [ ] **Step 1: Compile and fix all warnings**

Run: `stack build --fast 2>&1 | grep -i warn`

Fix any non-exhaustive pattern match warnings by adding cases for new constructors. Common locations:
- Any module that pattern-matches on `GlobalNode`, `LocalNode`, `ContextEdgeLabel`, `GlobalType`, `LocalType`, `Process`, `Expr`
- Modules: `Balanced.hs`, `Infer.hs`, `Typecheck.hs` (if they exist and pattern-match on these types)

- [ ] **Step 2: Run full test suite**

Run: `stack test 2>&1`

All existing tests should still pass. New payload tests should also pass.

- [ ] **Step 3: Commit**

```bash
git add -A
git commit -m "fix: resolve exhaustiveness warnings for payload type constructors"
```

---

## Chunk 6: Comprehensive Integration Tests

### Task 15: End-to-End Payload Tests

**Files:**
- Modify: `test/ProjectionSpec.hs`, `test/SubtypingSpec.hs`, `test/SynthesiseSpec.hs`

- [ ] **Step 1: Add comprehensive projection tests**

```haskell
describe "payload projection" $ do
  it "projects simple payload send/recv" $
    expectProjectionAs projectInductiveFull
      "p -> q [int]; end" "p" "q ! [int]; end"

  it "projects payload onto uninvolved participant" $
    expectProjectionAs projectInductiveFull
      "p -> q [int]; q -> r [bool]; end" "p" "q ! [int]; end"

  it "projects mixed payload and label" $
    expectProjectionAs projectInductiveFull
      "p -> q {l1: q -> r [int]; end, l2: q -> r [int]; end}"
      "r"
      "q ? [int]; end"

  it "rejects mixed payload types across branches" $
    expectProjectionFails projectInductiveFull
      "p -> q {l1: q -> r [int]; end, l2: q -> r [bool]; end}"
      "r"

  it "projects recursive payload protocol" $
    expectProjectionAs projectInductiveFull
      "rec t . p -> q [int]; t" "p" "rec t . q ! [int]; t"
```

- [ ] **Step 2: Add comprehensive synthesis round-trip tests**

```haskell
  it "round-trips payload protocol through synthesis" $
    expectSynthRoundtrips
      [("p", "q ! [int]; end"), ("q", "p ? [int]; end")]

  it "round-trips mixed payload and label protocol" $
    expectSynthRoundtrips
      [ ("p", "q ! {l1: end, l2: end}")
      , ("q", "p ? {l1: r ! [int]; end, l2: r ! [bool]; end}")
      , ("r", "q ? {l1: end, l2: end}")  -- wait, this won't work with payloads...
      ]
```

- [ ] **Step 3: Add subtyping tests for payload types**

```haskell
  it "payload send is subtype of itself" $
    expectSubtype "q ! [int]; end" "q ! [int]; end"

  it "payload recv is subtype of itself" $
    expectSubtype "q ? [int]; end" "q ? [int]; end"

  it "different payload types are not subtypes" $
    expectNotSubtype "q ! [int]; end" "q ! [bool]; end"
```

- [ ] **Step 4: Run full test suite**

Run: `stack test 2>&1`

- [ ] **Step 5: Final commit**

```bash
git add -A
git commit -m "test: add comprehensive payload type integration tests"
```
