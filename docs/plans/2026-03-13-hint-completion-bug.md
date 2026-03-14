# Hint Completion Bug in Graph-to-Type Reconstruction

## The Bug

Projecting the global type

```
μt.a→b:{
  m. c→d:{
    m. a→c:m.t
    m′. μt.a→c:m.a→b:{m.t, m′.μt.a→c:m.t}
  }
  m′. μt.c→d:{m.a→c:m.t, m′.μt.a→c:m.t}
}
```

onto participant **c** crashes with coinductive-full:

```
GraphToTypeInvalidGraph "Encountered local cycle at vertex 3 without a visible recursion variable hint."
```

Projections onto **a**, **b**, **d** succeed but produce spurious consecutive `rec` wrappers:

- a: `rec t1 . b!{m: rec t6 . c!{m: b!{m: t6, m': rec t4 . rec t3 . c!{m: t3}}}, m': rec t4 . c!{m: t4}}`
  - `rec t4 . rec t3 .` — t4 is unused
- b: `rec t1 . a?{m: rec t5 . a?{m: t5, m': rec t4 . rec t3 . end}, m': rec t4 . end}`
  - `rec t4 . rec t3 . end` — both unused
  - `rec t4 . end` — t4 unused
- d: `rec t1 . c?{m: t1, m': rec t2 . rec t5 . end}`
  - `rec t2 . rec t5 . end` — both unused

## Root Cause

Two related problems in the hint completion mechanism (`completeLocalHints` / `completeGlobalHints` in `Automata.hs`):

### 1. `ensureEntryHintForAncestorLocal` treats `preferredVar` as a binder

When a back-edge targets a vertex V, the function checks whether V's entry edge already has hints via `hasAnyRecVarHints`. This returns `True` if there's a `preferredVar`, even though only `rvhBinders` actually create `rec` wrappers and register variables in `activeNames`.

For participant c's vertex 3:
- Entry edge 2→3 has `{binders=[], preferredVar=Just t5}`
- `hasAnyRecVarHints` returns True → no binder added
- `localGraphToType` can't find a variable for vertex 3 → crash

**Partial fix applied**: Changed `ensureEntryHintForAncestorLocal` (and the global version) to check `null (rvhBinders existing)` instead of `not (hasAnyRecVarHints existing)`, promoting `preferredVar` to a binder when no binder exists. Same fix in `propagateLocalCyclicBinders` and `propagateGlobalCyclicBinders`. This fixes the crash.

### 2. Coinductive projection accumulates irrelevant rec-var hints

The coinductive-full projection accumulates `RecVarHints` via `appendHints` as it passes through global `rec` nodes during `closureFor` / `exploreCoind`. These hints include binders from global `rec` variables that aren't relevant to the projected participant's recursion structure.

Example: participant c's local graph edge 0→2 has `{binders=[t2, t5]}`. Both t2 and t5 come from global `rec` binders traversed along the projection path, but vertex 2 is NOT on any cycle in c's local graph. The binders produce dead `rec t2 . rec t5 . body` wrappers.

## The Local Graphs (debug output)

### Participant c (crashes)
```
Vertices: 0=d!{m,m'}, 1=a?{m}, 2=a?{m}, 3=a?{m}
Edges: 0→1(d!m), 0→2(d!m'), 1→0(a?m), 2→3(a?m), 3→3(a?m, self-loop)
Start hints: {binders=[t1]}
Edge 2→3 hints: {binders=[], preferred=Just t5}  ← MISSING BINDER
Edge 3→3 hints: {binders=[], preferred=Just t5}
```

### Participant a (spurious recs)
```
Vertices: 0=b!{m,m'}, 1=c!{m}, 2=c!{m}, 3=b!{m,m'}, 4=c!{m}
Edges: 0→1(b!m), 0→2(b!m'), 1→3(c!m), 2→2(self-loop), 3→1(b!m), 3→4(b!m'), 4→4(self-loop)
Edge 0→2 hints: {binders=[t4]}        ← t4 is irrelevant (vertex 2 is cyclic, but t4 is for something else)
Edge 3→4 hints: {binders=[t4, t3]}    ← t4 is irrelevant
```

## What Needs to Happen

The hint completion algorithm needs to ensure that:

1. Every vertex on a cycle has exactly one binder on its entry edge(s)
2. No vertex that is NOT on a cycle gets a binder
3. Binders correspond to actual recursion points in the local graph, not inherited from global rec variables that don't affect this participant

The current algorithm (`dfsCompleteLocal` + `buildLocalCyclicVarMap` + `propagateLocalCyclicBinders`) is overcomplicated. A simpler approach: determine which vertices are cyclic, assign each one a variable, and place binders only on edges entering those vertices — rather than inheriting and propagating hints from the global graph's rec structure.

## Files

- `src/Automata.hs`: `completeLocalHints`, `completeGlobalHints`, `localGraphToType`, `globalGraphToType`
- `src/Project.hs`: `exploreCoind` (where hints are accumulated during coinductive projection)

## Current State

- Partial fix applied to `ensureEntryHintForAncestorLocal` / `ensureEntryHintForAncestor` (binder check instead of `hasAnyRecVarHints`)
- Same fix applied to `propagateLocalCyclicBinders` / `propagateGlobalCyclicBinders`
- `freeTypeVarsLocal`, `freeTypeVarsGlobal`, `stripDeadLocalRecs`, `stripDeadGlobalRecs` added to `Syntax.AST` (may or may not be needed depending on approach)
- Roundtrip tests NOT modified (they should continue to pass)
- Tests not yet run after latest changes
