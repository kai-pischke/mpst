# Hint Completion Fix — Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix spurious `rec` wrappers and crash in coinductive/inductive projection by stopping the projection code from propagating global `rvhBinders` into local graph edges.

**Architecture:** The projection code (`Project.hs`) currently carries global `RecVarHints` (including binders from global `rec` nodes) through to local graph edges via `appendHints`. These binders don't correspond to local cycle structure, causing (a) dead `rec` wrappers and (b) crashes when `preferredVar` masks a missing binder. The fix: strip `rvhBinders` at the projection boundary — only pass `rvhPreferredVar` as a name hint. Let `completeLocalHints` in `Automata.hs` compute binders from the local graph's actual cycle structure.

**Tech Stack:** Haskell, Stack

---

### Task 1: Add failing tests for the bug

**Files:**
- Modify: `test/ProjectionSpec.hs`

- [ ] **Step 1: Add test for the crash (participant c)**

Add a test using the global type from the bug report that crashes when projecting onto participant c with coinductive-full. The expected local type for c should be a well-formed recursive type (no crash).

The global type:
```
rec t . a -> b { m: c -> d { m: a -> c : m . t, m': rec t2 . a -> c : m . a -> b { m: t, m': rec t3 . a -> c : m . t3 } }, m': rec t4 . c -> d { m: a -> c : m . t, m': rec t5 . a -> c : m . t5 } }
```

```haskell
it "[PROJ-HINT-001] coinductive-full projects onto c without crashing (hint completion bug)" $
  expectProjectionSucceeds
    projectCoinductiveFull
    gHintBug
    "c"
```

- [ ] **Step 2: Add tests for spurious rec wrappers (participants a, b, d)**

These projections succeed but produce dead `rec` wrappers. Add tests that check for the expected clean local types (no dead `rec` wrappers).

```haskell
it "[PROJ-HINT-002] coinductive-full projection onto a has no spurious recs" $
  expectProjectionAs
    projectCoinductiveFull
    gHintBug
    "a"
    expectedA

it "[PROJ-HINT-003] coinductive-full projection onto b has no spurious recs" $
  expectProjectionAs
    projectCoinductiveFull
    gHintBug
    "b"
    expectedB

it "[PROJ-HINT-004] coinductive-full projection onto d has no spurious recs" $
  expectProjectionAs
    projectCoinductiveFull
    gHintBug
    "d"
    expectedD
```

To determine the correct expected local types, project manually or use debug output after the fix. The key property: no `rec t . body` where `t` is not free in `body`.

- [ ] **Step 3: Add the global type fixture**

```haskell
gHintBug :: String
gHintBug =
  "rec t . a -> b { "
    ++ "m: c -> d { "
    ++ "  m: a -> c : m . t, "
    ++ "  m': rec t2 . a -> c : m . a -> b { m: t, m': rec t3 . a -> c : m . t3 } "
    ++ "}, "
    ++ "m': rec t4 . c -> d { "
    ++ "  m: a -> c : m . t, "
    ++ "  m': rec t5 . a -> c : m . t5 "
    ++ "} "
    ++ "}"
```

- [ ] **Step 4: Run tests to verify they fail**

Run: `stack test --ta '-m "PROJ-HINT"'`
Expected: HINT-001 fails with `GraphToTypeInvalidGraph` crash. HINT-002/003/004 may fail with mismatched types (spurious recs) or may need expected types adjusted after the fix.

- [ ] **Step 5: Commit**

```bash
git add test/ProjectionSpec.hs
git commit -m "test: add failing tests for hint completion bug in projection"
```

---

### Task 2: Strip binders at the projection boundary

**Files:**
- Modify: `src/Project.hs`

The core fix. Change `appendHints` to not propagate `rvhBinders` — only merge `rvhPreferredVar`. Also strip binders from global start hints when used as local start hints.

- [ ] **Step 1: Add `hintsToPreference` helper**

Add a function that strips binders from hints, keeping only the preferred var as a name suggestion. Place it near `appendHints`.

```haskell
-- | Strip structural binders, keeping only the preferred variable name.
-- Used at the projection boundary: global binders don't correspond to
-- local cycle structure — completeLocalHints will compute those.
hintsToPreference :: RecVarHints -> RecVarHints
hintsToPreference hints =
  emptyRecVarHints { rvhPreferredVar = rvhPreferredVar hints }
```

- [ ] **Step 2: Apply `hintsToPreference` in coinductive projection start hints**

In `projectCoinductiveFull` (line 64), strip binders from `ggStartVarHints gg`:

```haskell
-- Before:
materialiseLocalGraph (csBuild st) (ProjectionTarget 0 (ggStartVarHints gg))
-- After:
materialiseLocalGraph (csBuild st) (ProjectionTarget 0 (hintsToPreference (ggStartVarHints gg)))
```

- [ ] **Step 3: Apply `hintsToPreference` in coinductive `appendHints` call sites**

In `analyseClosedState` / `mkTransition` (lines 321, 395), strip binders from the global edge hints before folding:

```haskell
-- Line 321 (payload):
hints = foldl' appendHints emptyHints (fmap (hintsToPreference . gpeTargetHints . fst . snd) edges)

-- Line 395 (branch):
hints = foldl' appendHints emptyHints (fmap (hintsToPreference . geTargetHints . fst) picks)
```

- [ ] **Step 4: Apply `hintsToPreference` in inductive projection**

In `projectAt` / `projectIgnored` / `projectInvolved` / `projectPayloadNode`, strip binders from global edge hints where they feed into local graph hints:

- Line 522 (`projectInvolved`): `projectAt env Set.empty (hintsToPreference (geTargetHints edgeLbl)) child childStart`
- Line 547 (`projectIgnored`): `appendHints hints (hintsToPreference (geTargetHints edgeLbl))`
- Line 604 (`projectPayloadNode` send): `projectAt env Set.empty (hintsToPreference (gpeTargetHints edgeLbl)) child childStart`
- Line 610 (`projectPayloadNode` recv): same
- Line 615 (`projectPayloadNode` uninvolved): `projectAt env Set.empty (appendHints hints (hintsToPreference (gpeTargetHints edgeLbl))) child lv`

Additionally, the inductive projection's `materialiseLocalGraph` call uses `ptHints target` for `lgStartVarHints`. The `ptHints` comes from `projectAt` which already passes through `hintsToPreference`-filtered hints, so the start hints should be clean. But verify: the initial call to `projectAt` (in `projectInductiveWith`) passes the global start hints — that also needs `hintsToPreference`.

- [ ] **Step 5: Run tests**

Run: `stack test`
Expected: The PROJ-HINT-001 crash test now passes. Existing projection tests continue to pass. Round-trip tests (`AUTO2TYPE-*-PROP`) still pass (they go through `buildLocalGraph` → `entryHintsLocal`, not projection).

- [ ] **Step 6: Commit**

```bash
git add src/Project.hs
git commit -m "fix: strip global binders at projection boundary, keep only preferredVar"
```

---

### Task 3: Determine and fix expected local types for hint tests

**Files:**
- Modify: `test/ProjectionSpec.hs`

After the fix, the projected types should have no spurious `rec` wrappers.

- [ ] **Step 1: Inspect actual projected types**

Temporarily add debug output or use GHCi to project the bug fixture onto a, b, c, d and inspect the resulting local types. These should now be clean.

- [ ] **Step 2: Update expected types in PROJ-HINT-002/003/004**

Fill in the correct expected local types based on the actual clean output. Verify each has no dead `rec` binders.

- [ ] **Step 3: Run all tests**

Run: `stack test`
Expected: All 256+ tests pass, including the new PROJ-HINT tests.

- [ ] **Step 4: Commit**

```bash
git add test/ProjectionSpec.hs
git commit -m "test: fill in expected local types for hint completion tests"
```

---

### Task 4: Clean up dead code

**Files:**
- Modify: `src/Automata.hs` (potentially)
- Modify: `src/Syntax/AST.hs` (potentially)

- [ ] **Step 1: Check if `stripDeadLocalRecs` / `stripDeadGlobalRecs` are still needed**

These were added as a potential workaround. If the fix makes them unnecessary and they're unused, remove them.

- [ ] **Step 2: Check if `hasAnyRecVarHints` is still needed**

The partial fix changed call sites to use `null (rvhBinders ...)` directly. If `hasAnyRecVarHints` is now unused, remove it.

- [ ] **Step 3: Run tests**

Run: `stack test`
Expected: All tests pass.

- [ ] **Step 4: Commit**

```bash
git add src/Automata.hs src/Syntax/AST.hs
git commit -m "refactor: remove unused hint helpers"
```

---

### Task 5: Strengthen round-trip test

**Files:**
- Modify: `test/AutomataReconstructionSpec.hs`

- [ ] **Step 1: Remove canonicalization from round-trip tests**

Already done in this conversation — verify the `reconstructed === g` / `reconstructed === l` comparisons are committed. Remove the now-unused `canonicalizeGlobalHintNames`, `canonicalizeLocalHintNames`, `renameHintList`, `renameMaybeHint`, `renameRecVarHints` functions.

- [ ] **Step 2: Run tests**

Run: `stack test`
Expected: All tests pass.

- [ ] **Step 3: Commit**

```bash
git add test/AutomataReconstructionSpec.hs
git commit -m "test: strengthen roundtrip tests to exact equality, remove canonicalization"
```
