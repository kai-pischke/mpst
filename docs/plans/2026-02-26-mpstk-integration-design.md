# mpstk Integration Design

## Goal

Add an optional mpstk backend for verifying session type properties (safety, deadlock-freedom, liveness) by shelling out to the `mpstk` CLI tool, which uses mCRL2 model checking internally.

## Decisions

- **Translation source:** `Map Participant LocalType` pairs. When starting from a `ContextGraph`, reconstruct local types via `localGraphToType` first.
- **Payloads:** Omitted (mpstk defaults to unit).
- **Session name:** Fixed `s` for all participants.
- **Properties:** safety, deadlock-freedom (df), and live+ from mpstk.  Note: mpstk's live+ is strictly stronger than our native `checkLiveness` — it checks per-message-label liveness, which in practice requires safety as well.
- **Return type:** `Bool` per property.
- **Approach:** Single mpstk invocation returns all results; per-property functions project from the result.

## Syntax Translation

| This project | mpstk |
|---|---|
| `p ! {l1: T1, l2: T2}` | `p (+) {l1 . T1, l2 . T2}` |
| `p ? {l1: T1, l2: T2}` | `p & {l1 . T1, l2 . T2}` |
| `rec t . T` | `rec(t) T` |
| `t` | `t` |
| `end` | `end` |

Context format: `s[participant]: translatedLocalType, ...`

## Module: `src/MpstkBackend.hs`

```haskell
data MpstkResults = MpstkResults
  { mpstkSafe :: Bool
  , mpstkDeadlockFree :: Bool
  , mpstkLivePlus :: Bool
  }

toMpstkCtx :: Map Participant LocalType -> String
mpstkVerify :: Map Participant LocalType -> IO MpstkResults
mpstkCheckSafety :: Map Participant LocalType -> IO Bool
mpstkCheckDeadlockFreedom :: Map Participant LocalType -> IO Bool
mpstkCheckLivePlus :: Map Participant LocalType -> IO Bool
```

## Flow

1. `toMpstkCtx` translates `Map Participant LocalType` to mpstk `.ctx` string
2. `mpstkVerify` writes the string to a temp file
3. Calls `mpstk verify <tempfile>` via `System.Process.readProcess`
4. Parses the tab-separated output table
5. Extracts `safe`, `df`, and `live+` columns into `MpstkResults`

## Error Handling

`IOException` if mpstk is not on PATH or returns non-zero exit. No recovery -- caller should fall back to native checks.

## Testing (`test/MpstkBackendSpec.hs`)

1. Unit tests for `toMpstkCtx` -- verify syntax translation (no mpstk required)
2. Integration tests for `mpstkVerify` -- cross-validate with native check results on shared test contexts (requires mpstk installed)
