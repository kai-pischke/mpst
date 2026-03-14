# mpstk Integration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add an mpstk backend that translates `Map Participant LocalType` to mpstk `.ctx` format and shells out to `mpstk verify` to check safety, deadlock-freedom, and live+ (mpstk's per-label liveness, which is strictly stronger than our native `checkLiveness`).

**Architecture:** New module `src/MpstkBackend.hs` with a pure syntax translator (`toMpstkCtx`) and IO-based verifier (`mpstkVerify`). Uses `process` and `temporary` packages. Single mpstk invocation returns all property results.

**Tech Stack:** Haskell, `System.Process.readProcess`, `System.IO.Temp`, hspec for tests.

---

### Task 1: Add dependencies to package.yaml

**Files:**
- Modify: `package.yaml`

**Step 1: Add `process` and `temporary` to dependencies**

In `package.yaml`, add `process` and `temporary` to the top-level `dependencies` list:

```yaml
dependencies:
  - base >= 4.14 && < 5
  - containers
  - megaparsec
  - prettyprinter
  - transformers
  - mtl
  - array
  - graphviz
  - text
  - process
  - temporary
```

**Step 2: Add `MpstkBackend` to exposed-modules**

In `package.yaml` under `library.exposed-modules`, add:

```yaml
  exposed-modules:
    - MPST
    - Syntax
    - Automata
    - Balanced
    - DeadlockFreedom
    - Merge
    - MpstkBackend
    - Project
    - Safety
    - Subtyping
    - Liveness
    - Visualise
```

**Step 3: Add `MpstkBackendSpec` to test other-modules**

In `package.yaml` under `tests.mpst-test.other-modules`, add `MpstkBackendSpec`.

**Step 4: Verify it builds**

Run: `stack build 2>&1 | tail -5`
Expected: Build succeeds (module won't exist yet, so this step is deferred until after Task 2).

---

### Task 2: Write failing tests for `toMpstkLocalType` (syntax translation)

**Files:**
- Create: `test/MpstkBackendSpec.hs`

**Step 1: Write the failing tests**

```haskell
module MpstkBackendSpec (spec) where

import qualified Data.Map.Strict as Map
import Syntax (Label(..), LocalType(..), Participant(..), TypeVar(..), parseLocalTypeChecked)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)
import MpstkBackend (toMpstkLocalType, toMpstkCtx)

spec :: Spec
spec =
  describe "mpstk backend" $ do
    describe "toMpstkLocalType" $ do
      it "translates end" $
        toMpstkLocalType LEnd `shouldBe` "end"

      it "translates a type variable" $
        toMpstkLocalType (LVar (TypeVar "t")) `shouldBe` "t"

      it "translates a single-branch send" $
        expectTranslation
          "q ! {ok: end}"
          "q (+) ok . end"

      it "translates a multi-branch send" $
        expectTranslation
          "q ! {l1: end, l2: end}"
          "q (+) {l1 . end, l2 . end}"

      it "translates a single-branch receive" $
        expectTranslation
          "q ? {ok: end}"
          "q & ok . end"

      it "translates a multi-branch receive" $
        expectTranslation
          "q ? {l1: end, l2: end}"
          "q & {l1 . end, l2 . end}"

      it "translates recursion" $
        expectTranslation
          "rec t . q ! {go: t}"
          "rec(t) q (+) go . t"

      it "translates nested send/receive" $
        expectTranslation
          "q ! {k: r ? {ack: end}}"
          "q (+) k . r & ack . end"

    describe "toMpstkCtx" $ do
      it "translates a two-party context" $
        expectCtxTranslation
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l2: end}")]
          "s[p]: p (+) {l1 . end, l2 . end},\ns[q]: q & {l1 . end, l2 . end}"

-- Helpers

expectTranslation :: String -> String -> IO ()
expectTranslation input expected =
  case parseLocalTypeChecked input of
    Left err -> expectationFailure ("parse error: " ++ err)
    Right lt -> toMpstkLocalType lt `shouldBe` expected

expectCtxTranslation :: [(String, String)] -> String -> IO ()
expectCtxTranslation pairs expected = do
  let parsed = mapM parsePair pairs
  case parsed of
    Left err -> expectationFailure ("parse error: " ++ err)
    Right entries -> toMpstkCtx (Map.fromList entries) `shouldBe` expected
  where
    parsePair (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left err
        Right lt -> Right (Participant name, lt)
```

Note: The `toMpstkCtx` test for the two-party context has a subtle issue — `toMpstkCtx` takes `Map Participant LocalType`, and we're feeding it the local types with the *peer* participant. The send `q ! {l1: end, l2: end}` for participant `p` translates to `q (+) {l1 . end, l2 . end}` — the peer name `q` comes from the AST, not the map key. This is correct.

**Step 2: Run tests to verify they fail**

Run: `stack test 2>&1 | tail -20`
Expected: Compilation error — `MpstkBackend` module not found.

---

### Task 3: Implement `toMpstkLocalType` and `toMpstkCtx`

**Files:**
- Create: `src/MpstkBackend.hs`

**Step 1: Write the implementation**

```haskell
module MpstkBackend
  ( MpstkResults(..)
  , toMpstkLocalType
  , toMpstkCtx
  , mpstkVerify
  , mpstkCheckSafety
  , mpstkCheckDeadlockFreedom
  , mpstkCheckLiveness
  ) where

import qualified Data.List.NonEmpty as NE
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Syntax.AST (Label(..), LocalType(..), Participant(..), TypeVar(..))

-- | Results from mpstk verification.
data MpstkResults = MpstkResults
  { mpstkSafe :: !Bool
  , mpstkDeadlockFree :: !Bool
  , mpstkLive :: !Bool
  } deriving (Eq, Show)

-- | Translate a local type to mpstk syntax.
toMpstkLocalType :: LocalType -> String
toMpstkLocalType LEnd = "end"
toMpstkLocalType (LVar (TypeVar v)) = v
toMpstkLocalType (LRec (TypeVar v) body) =
  "rec(" ++ v ++ ") " ++ toMpstkLocalType body
toMpstkLocalType (LSend (Participant p) branches) =
  p ++ " (+) " ++ renderBranches branches
toMpstkLocalType (LRecv (Participant p) branches) =
  p ++ " & " ++ renderBranches branches

renderBranches :: NE.NonEmpty (Label, LocalType) -> String
renderBranches branches
  | NE.length branches == 1 =
      let (Label l, cont) = NE.head branches
       in l ++ " . " ++ toMpstkLocalType cont
  | otherwise =
      "{" ++ intercalate ", " (map renderBranch (NE.toList branches)) ++ "}"
  where
    renderBranch (Label l, cont) = l ++ " . " ++ toMpstkLocalType cont

-- | Translate a context (participant -> local type) to mpstk .ctx format.
toMpstkCtx :: Map.Map Participant LocalType -> String
toMpstkCtx ctx =
  intercalate ",\n" entries
  where
    entries =
      [ "s[" ++ getParticipant p ++ "]: " ++ toMpstkLocalType lt
      | (p, lt) <- Map.toAscList ctx
      ]

-- Stubs for IO functions (implemented in Task 5)
mpstkVerify :: Map.Map Participant LocalType -> IO MpstkResults
mpstkVerify = error "mpstkVerify: not yet implemented"

mpstkCheckSafety :: Map.Map Participant LocalType -> IO Bool
mpstkCheckSafety = error "mpstkCheckSafety: not yet implemented"

mpstkCheckDeadlockFreedom :: Map.Map Participant LocalType -> IO Bool
mpstkCheckDeadlockFreedom = error "mpstkCheckDeadlockFreedom: not yet implemented"

mpstkCheckLiveness :: Map.Map Participant LocalType -> IO Bool
mpstkCheckLiveness = error "mpstkCheckLiveness: not yet implemented"
```

**Step 2: Run the translation tests**

Run: `stack test 2>&1 | tail -30`
Expected: All `toMpstkLocalType` and `toMpstkCtx` tests pass. IO tests not yet written.

**Step 3: Commit**

```bash
git add src/MpstkBackend.hs test/MpstkBackendSpec.hs package.yaml
git commit -m "add mpstk syntax translation"
```

---

### Task 4: Write failing tests for `mpstkVerify`

**Files:**
- Modify: `test/MpstkBackendSpec.hs`

**Step 1: Add integration tests**

Append to the `spec` in `MpstkBackendSpec.hs`, inside the top-level `describe`:

```haskell
    describe "mpstkVerify (integration)" $ do
      it "reports safe + deadlock-free + live for matched send/receive loop" $
        expectMpstkResults
          [("p", "rec t . q ! {a: t}"), ("q", "rec t . p ? {a: t}")]
          MpstkResults { mpstkSafe = True, mpstkDeadlockFree = True, mpstkLive = True }

      it "reports unsafe for mismatched labels" $
        expectMpstkResults
          [("p", "q ! {l1: end, l2: end}"), ("q", "p ? {l1: end, l3: end}")]
          MpstkResults { mpstkSafe = False, mpstkDeadlockFree = False, mpstkLive = False }

      it "reports safe but not deadlock-free for one-sided termination" $
        expectMpstkResults
          [("p", "q ! {l1: q ! {l2: end}}"), ("q", "p ? {l1: end}")]
          MpstkResults { mpstkSafe = True, mpstkDeadlockFree = False, mpstkLive = False }

      it "reports safe + deadlock-free + live for simple terminating protocol" $
        expectMpstkResults
          [("p", "q ! {ok: end}"), ("q", "p ? {ok: end}")]
          MpstkResults { mpstkSafe = True, mpstkDeadlockFree = True, mpstkLive = True }
```

Add helper:

```haskell
expectMpstkResults :: [(String, String)] -> MpstkResults -> IO ()
expectMpstkResults pairs expected = do
  let parsed = mapM parsePair pairs
  case parsed of
    Left err -> expectationFailure ("parse error: " ++ err)
    Right entries -> do
      results <- mpstkVerify (Map.fromList entries)
      results `shouldBe` expected
  where
    parsePair (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left err
        Right lt -> Right (Participant name, lt)
```

Also add `MpstkResults` and `mpstkVerify` to the import of `MpstkBackend`.

**Step 2: Run tests to verify they fail**

Run: `stack test 2>&1 | tail -20`
Expected: The integration tests fail with "mpstkVerify: not yet implemented".

---

### Task 5: Implement `mpstkVerify` and convenience functions

**Files:**
- Modify: `src/MpstkBackend.hs`

**Step 1: Replace the stubs with real implementations**

Add these imports at the top of `MpstkBackend.hs`:

```haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hFlush, hClose)
import System.Process (readProcess)
```

Replace the stub implementations:

```haskell
-- | Run mpstk verify on a context. Requires mpstk on PATH.
mpstkVerify :: Map.Map Participant LocalType -> IO MpstkResults
mpstkVerify ctx = do
  let ctxStr = toMpstkCtx ctx
  withSystemTempFile "mpst.ctx" $ \path handle -> do
    hPutStr handle ctxStr
    hFlush handle
    hClose handle
    output <- readProcess "mpstk" ["verify", path] ""
    pure (parseMpstkOutput output)

-- | Parse mpstk's tab-separated output table.
parseMpstkOutput :: String -> MpstkResults
parseMpstkOutput output =
  case dropWhile isHeaderLine (lines output) of
    (dataLine : _) ->
      let fields = words dataLine
          -- Output columns: protocol df live live+ live++ nterm safe term
          -- fields after splitting: [path, df, live, live+, live++, nterm, safe, term]
       in case drop 1 fields of  -- drop the file path
            (df : _live : livePlus : _livePP : _nterm : safe : _term : _) ->
              MpstkResults
                { mpstkSafe = safe == "true"
                , mpstkDeadlockFree = df == "true"
                , mpstkLive = livePlus == "true"
                }
            _ -> error ("mpstk: unexpected output format: " ++ output)
    [] -> error ("mpstk: no data line in output: " ++ output)
  where
    isHeaderLine l = "protocol" `isPrefixOf` l || "Legend:" `isPrefixOf` l || " *" `isPrefixOf` l || null l

mpstkCheckSafety :: Map.Map Participant LocalType -> IO Bool
mpstkCheckSafety ctx = mpstkSafe <$> mpstkVerify ctx

mpstkCheckDeadlockFreedom :: Map.Map Participant LocalType -> IO Bool
mpstkCheckDeadlockFreedom ctx = mpstkDeadlockFree <$> mpstkVerify ctx

mpstkCheckLiveness :: Map.Map Participant LocalType -> IO Bool
mpstkCheckLiveness ctx = mpstkLive <$> mpstkVerify ctx
```

Add `isPrefixOf` import: `import Data.List (intercalate, isPrefixOf)`.

**Step 2: Run all tests**

Run: `stack test 2>&1 | tail -30`
Expected: All tests pass (both translation unit tests and mpstk integration tests).

**Step 3: Commit**

```bash
git add src/MpstkBackend.hs test/MpstkBackendSpec.hs
git commit -m "add mpstk verification backend"
```

---

### Task 6: Wire into MPST re-export module and add to test runner

**Files:**
- Modify: `src/MPST.hs`
- Modify: `test/Spec.hs`

**Step 1: Add MpstkBackend to MPST.hs re-exports**

Add `MpstkBackend` to the import and re-export list in `src/MPST.hs`:

```haskell
module MPST
  ( module Syntax
  , module Automata
  , module Balanced
  , module DeadlockFreedom
  , module MpstkBackend
  , module Project
  , module Safety
  , module Subtyping
  , module Liveness
  , module Visualise
  ) where

...
import MpstkBackend
```

**Step 2: Add MpstkBackendSpec to test/Spec.hs**

Add `import qualified MpstkBackendSpec` and `MpstkBackendSpec.spec` to the hspec block.

**Step 3: Run full test suite**

Run: `stack test 2>&1 | tail -30`
Expected: All tests pass including MpstkBackendSpec.

**Step 4: Commit**

```bash
git add src/MPST.hs test/Spec.hs
git commit -m "wire mpstk backend into MPST module"
```

---

### Task 7: Final verification

**Step 1: Run full test suite one more time**

Run: `stack test 2>&1`
Expected: All tests pass.

**Step 2: Verify mpstk integration manually**

Run: `stack ghci` and test interactively:

```haskell
import qualified Data.Map.Strict as Map
import MpstkBackend
import Syntax

let Right p = parseLocalTypeChecked "rec t . q ! {a: t}"
let Right q = parseLocalTypeChecked "rec t . p ? {a: t}"
putStrLn (toMpstkCtx (Map.fromList [(Participant "p", p), (Participant "q", q)]))
mpstkVerify (Map.fromList [(Participant "p", p), (Participant "q", q)])
```

Expected: Prints the `.ctx` format and `MpstkResults {mpstkSafe = True, mpstkDeadlockFree = True, mpstkLive = True}`.
