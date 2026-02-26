# GlobalGraph Synthesis from ContextGraph Design

## Goal

Synthesise a GlobalGraph from a ContextGraph using a round-robin priority-based traversal of send-active participants.

## Algorithm

```
synthesise(contextNode, prioritisedIdx, env):
  1. Find next send-active participant starting from prioritisedIdx
     (cycling through cgParticipants). Send-active = sender in some
     ContextSyncEdge from contextNode.
  2. No one send-active → emit GlobalEndNode.
  3. If (next, contextNode) ∈ env → return env[(next, contextNode)] (back-edge).
  4. Otherwise:
     - Create fresh GlobalNode gNode
     - env' = env ∪ {(next, contextNode) → gNode}
     - Collect all ContextSyncEdges where ceSender == next
     - Error if multiple receivers
     - For each edge (label l, target contextNode'):
         recurse with (contextNode', (nextIdx+1) mod #participants, env')
     - Return gNode
```

## Decisions

- **Input:** `ContextGraph` directly. Callers build it themselves.
- **Env key:** `(Participant, G.Vertex)` — the send-active participant and context graph vertex.
- **RecVarHints:** Empty during synthesis. `completeGlobalHints` fills them in.
- **Multiple receivers:** Error — a send-active participant should only target one receiver per context state.
- **Priority rotation:** After emitting for participant at index i, recurse with priority i+1 mod n.

## Module: `src/Synthesise.hs`

```haskell
data SynthesisError
  = NoSendActiveParticipant G.Vertex
  | MultipleReceivers G.Vertex Participant
  | InternalError String

synthesise :: ContextGraph -> Either SynthesisError GlobalGraph
```

## Testing (`test/SynthesiseSpec.hs`)

1. **Unit tests** — hand-crafted contexts with known expected globals
2. **Roundtrip** — global → project → context → synthesise → project → supertype check
3. **Correctness property** — safe+live contexts → synthesise → project with coinductive-full → checkContextSubtype against original
