-- | Synthesis of a global graph from a context graph.
--
-- The algorithm traverses the context automaton and constructs a global
-- automaton by identifying send-active participants in round-robin order.
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
  , RecVarHints(..)
  )
import Data.Array (array)
import Data.Foldable (foldl')
import Data.List (nub, sortOn)
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import Syntax.AST (Participant)

-- | Errors that can occur during synthesis.
data SynthesisError
  = MultipleReceivers G.Vertex Participant
  | InternalError String
  deriving (Eq, Show)

-- | Internal state threaded through the synthesis traversal.
data SynthState = SynthState
  { ssNextVertex :: !G.Vertex
  , ssNodes :: Map.Map G.Vertex GlobalNode
  , ssEdges :: [((G.Vertex, G.Vertex), GlobalEdgeLabel)]
  , ssEnv :: Map.Map (Participant, G.Vertex) G.Vertex
  }

emptySynthState :: SynthState
emptySynthState = SynthState 0 Map.empty [] Map.empty

freshVertex :: GlobalNode -> SynthState -> (G.Vertex, SynthState)
freshVertex node st =
  let v = ssNextVertex st
   in ( v
      , st
          { ssNextVertex = v + 1
          , ssNodes = Map.insert v node (ssNodes st)
          }
      )

addSynthEdge :: G.Vertex -> G.Vertex -> GlobalEdgeLabel -> SynthState -> SynthState
addSynthEdge from to lbl st =
  st { ssEdges = ((from, to), lbl) : ssEdges st }

emptyRecVarHints :: RecVarHints
emptyRecVarHints = RecVarHints [] Nothing

-- | Synthesise a global graph from a context graph.
synthesise :: ContextGraph -> Either SynthesisError GlobalGraph
synthesise cg = do
  let participants = cgParticipants cg
      outgoing = collectContextOutgoing (cgEdgeLabels cg)
  (startV, finalState) <-
    synthNode cg participants outgoing (cgStart cg) 0 emptySynthState
  pure (finaliseGlobalGraph startV finalState)

-- | Collect outgoing edges grouped by source vertex.
collectContextOutgoing :: Map.Map G.Edge [ContextEdgeLabel] -> Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)]
collectContextOutgoing =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(to, lbl)] m) acc labels)
    Map.empty

-- | Find the next send-active participant starting from the given priority index.
--
-- A participant is send-active at a vertex if they appear as @ceSender@ in
-- any @ContextSyncEdge@ from that vertex.
findSendActive ::
  [Participant] ->
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  G.Vertex ->
  Int ->
  Maybe (Participant, Int)
findSendActive participants outgoing contextVertex priorityIdx =
  let n = length participants
      indices = take n [priorityIdx .. priorityIdx + n - 1]
      edges = Map.findWithDefault [] contextVertex outgoing
      senders = foldl' collectSenders mempty edges
   in tryIndices n indices senders
  where
    collectSenders acc (_, ContextSyncEdge {ceSender = s}) = s : acc
    collectSenders acc _ = acc

    tryIndices _ [] _ = Nothing
    tryIndices n (i : is) senders =
      let idx = i `mod` n
          p = participants !! idx
       in if p `elem` senders
            then Just (p, idx)
            else tryIndices n is senders

-- | Core recursive synthesis: traverse a context node and build the global graph.
synthNode ::
  ContextGraph ->
  [Participant] ->
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  G.Vertex ->
  Int ->
  SynthState ->
  Either SynthesisError (G.Vertex, SynthState)
synthNode cg participants outgoing contextVertex priorityIdx st =
  case findSendActive participants outgoing contextVertex priorityIdx of
    Nothing ->
      -- No send-active participant: emit end node.
      let (v, st') = freshVertex GlobalEndNode st
       in Right (v, st')
    Just (sender, senderIdx) ->
      -- Check for back-edge (already visited with this sender at this context node).
      case Map.lookup (sender, contextVertex) (ssEnv st) of
        Just existingV ->
          -- Back-edge for recursion.
          Right (existingV, st)
        Nothing -> do
          -- Create a fresh global node.
          let (gNode, st1) = freshVertex GlobalNode st
              st2 = st1 { ssEnv = Map.insert (sender, contextVertex) gNode (ssEnv st1) }
          -- Collect all sync edges where this participant is the sender.
          let edges = Map.findWithDefault [] contextVertex outgoing
              syncEdges =
                [ (to, lbl)
                | (to, lbl@ContextSyncEdge{}) <- edges
                , ceSender lbl == sender
                ]
          -- Sort by label for deterministic output.
          let sortedEdges = sortOn (\(_, lbl) -> ceLabel lbl) syncEdges
          -- Verify: all edges must have the same receiver.
          let receivers = nub [ceReceiver lbl | (_, lbl) <- sortedEdges]
          case receivers of
            [] ->
              Left (InternalError ("No sync edges found for send-active participant " ++ show sender ++ " at vertex " ++ show contextVertex))
            [receiver] -> do
              -- Recurse for each branch edge.
              let n = length participants
                  nextPriority = (senderIdx + 1) `mod` n
              st3 <- foldlME (processBranch cg participants outgoing gNode sender receiver nextPriority) st2 sortedEdges
              Right (gNode, st3)
            _ ->
              Left (MultipleReceivers contextVertex sender)

-- | Process a single branch edge during synthesis.
processBranch ::
  ContextGraph ->
  [Participant] ->
  Map.Map G.Vertex [(G.Vertex, ContextEdgeLabel)] ->
  G.Vertex ->
  Participant ->
  Participant ->
  Int ->
  SynthState ->
  (G.Vertex, ContextEdgeLabel) ->
  Either SynthesisError SynthState
processBranch cg participants outgoing fromNode sender receiver nextPriority st (targetContextV, edgeLbl) = do
  (targetGlobalV, st') <-
    synthNode cg participants outgoing targetContextV nextPriority st
  let globalEdgeLbl = GlobalEdgeLabel
        { geSender = sender
        , geReceiver = receiver
        , geLabel = ceLabel edgeLbl
        , geTargetHints = emptyRecVarHints
        }
  pure (addSynthEdge fromNode targetGlobalV globalEdgeLbl st')

-- | Build the final GlobalGraph from the synthesis state.
finaliseGlobalGraph :: G.Vertex -> SynthState -> GlobalGraph
finaliseGlobalGraph startV st =
  let n = ssNextVertex st
      bounds = (0, n - 1)
      graph = G.buildG bounds (map fst (ssEdges st))
      nodeTable = array bounds (Map.toList (ssNodes st))
      edgeLabels = collectEdges (ssEdges st)
   in GlobalGraph
        { ggGraph = graph
        , ggStart = startV
        , ggNodes = nodeTable
        , ggEdgeLabels = edgeLabels
        , ggStartVarHints = emptyRecVarHints
        }

-- | Group edges by their key.
collectEdges :: Ord k => [(k, v)] -> Map.Map k [v]
collectEdges = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty

-- | Left fold with monadic accumulator (Either-based).
foldlME :: (b -> a -> Either e b) -> b -> [a] -> Either e b
foldlME _ acc [] = Right acc
foldlME f acc (x : xs) = do
  acc' <- f acc x
  foldlME f acc' xs
