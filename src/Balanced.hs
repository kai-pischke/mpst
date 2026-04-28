{-# LANGUAGE DeriveGeneric #-}

-- | Balancedness checking for global graphs.
module Balanced
  ( BalancedError(..)
  , BalancedResult
  , checkBalanced
  , checkWeakBalanced
  ) where

import Automata
  ( GlobalEdgeLabel(..)
  , GlobalGraph(..)
  , GlobalPayloadEdgeLabel(..)
  )
import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import GHC.Generics (Generic)
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST (Participant)

type ParticipantSet = Set.Set Participant

-- | Errors reported by balancedness checking.
data BalancedError = UnbalancedVertex
  { beVertex :: !G.Vertex
  , beMustParticipants :: ParticipantSet
  , beMayParticipants :: ParticipantSet
  }
  deriving (Eq, Show, Generic)

instance NFData BalancedError

-- | Result of balancedness checking.
type BalancedResult = Either [BalancedError] ()

-- | Check whether a global graph satisfies the balancedness criterion.
--
-- For each reachable node @v@:
--
-- * @must(v) = own(v) ∪ ⋂ must(children(v))@
-- * @may(v)  = own(v) ∪ ⋃ may(children(v))@
--
-- where @own(v)@ contains sender/receiver participants enabled at @v@.
--
-- A graph is balanced iff @must(v) = may(v)@ for every reachable node.
checkBalanced :: GlobalGraph -> BalancedResult
checkBalanced gg =
  case violations of
    [] -> Right ()
    errs -> Left errs
  where
    reachable = reachableVertices gg
    own = ownParticipants gg reachable
    children = reachableChildren gg reachable
    must0 = Map.fromSet (\v -> Map.findWithDefault Set.empty v own) reachable
    may0 = must0
    (mustFinal, mayFinal) = iterateToFixpoint own children must0 may0
    violations =
      [ UnbalancedVertex v mustSet maySet
      | v <- Set.toList reachable
      , let mustSet = Map.findWithDefault Set.empty v mustFinal
      , let maySet = Map.findWithDefault Set.empty v mayFinal
      , mustSet /= maySet
      ]

reachableVertices :: GlobalGraph -> Set.Set G.Vertex
reachableVertices gg =
  Set.fromList (G.reachable (ggGraph gg) (ggStart gg))

ownParticipants ::
  GlobalGraph ->
  Set.Set G.Vertex ->
  Map.Map G.Vertex ParticipantSet
ownParticipants gg reachable =
  foldl' addPayloadParticipants
    (foldl' addEdgeParticipants initial (Map.toList (ggEdgeLabels gg)))
    (Map.toList (ggPayloadEdges gg))
  where
    initial = Map.fromSet (const Set.empty) reachable

    addEdgeParticipants acc ((from, _), labels)
      | from `Set.member` reachable =
          let participants =
                Set.fromList
                  [ p
                  | lbl <- labels
                  , p <- [geSender lbl, geReceiver lbl]
                  ]
           in Map.insertWith Set.union from participants acc
      | otherwise = acc

    addPayloadParticipants acc ((from, _), labels)
      | from `Set.member` reachable =
          let participants =
                Set.fromList
                  [ p
                  | lbl <- labels
                  , p <- [gpeSender lbl, gpeReceiver lbl]
                  ]
           in Map.insertWith Set.union from participants acc
      | otherwise = acc

reachableChildren ::
  GlobalGraph ->
  Set.Set G.Vertex ->
  Map.Map G.Vertex (Set.Set G.Vertex)
reachableChildren gg reachable =
  foldl' addPayloadEdge
    (foldl' addEdge initial (Map.toList (ggEdgeLabels gg)))
    (Map.toList (ggPayloadEdges gg))
  where
    initial = Map.fromSet (const Set.empty) reachable

    addEdge acc ((from, to), labels)
      | from `Set.member` reachable
          && to `Set.member` reachable
          && not (null labels) =
          Map.insertWith Set.union from (Set.singleton to) acc
      | otherwise = acc

    addPayloadEdge acc ((from, to), labels)
      | from `Set.member` reachable
          && to `Set.member` reachable
          && not (null labels) =
          Map.insertWith Set.union from (Set.singleton to) acc
      | otherwise = acc

iterateToFixpoint ::
  Map.Map G.Vertex ParticipantSet ->
  Map.Map G.Vertex (Set.Set G.Vertex) ->
  Map.Map G.Vertex ParticipantSet ->
  Map.Map G.Vertex ParticipantSet ->
  (Map.Map G.Vertex ParticipantSet, Map.Map G.Vertex ParticipantSet)
iterateToFixpoint own children must may =
  if must == must' && may == may'
    then (must, may)
    else iterateToFixpoint own children must' may'
  where
    must' = Map.mapWithKey updateMust own
    may' = Map.mapWithKey updateMay own

    updateMust v ownSet =
      ownSet `Set.union` intersectChildren must v

    updateMay v ownSet =
      ownSet `Set.union` unionChildren may v

    intersectChildren current v =
      case Set.toList (Map.findWithDefault Set.empty v children) of
        [] -> Set.empty
        c : cs ->
          foldl'
            Set.intersection
            (Map.findWithDefault Set.empty c current)
            [Map.findWithDefault Set.empty child current | child <- cs]

    unionChildren current v =
      foldl'
        Set.union
        Set.empty
        [ Map.findWithDefault Set.empty child current
        | child <- Set.toList (Map.findWithDefault Set.empty v children)
        ]

-- ---------------------------------------------------------------------------
-- Weak balancedness
-- ---------------------------------------------------------------------------

-- | An unordered pair of participants.
type ParticipantPair = Set.Set Participant

-- | A set of participant pairs.
type PairSet = Set.Set ParticipantPair

-- | Check whether a global graph satisfies the weak balancedness criterion.
--
-- For each reachable node @v@, define @ready(v)@ as the set of participant
-- pairs that are "ready to communicate":
--
-- * If @v@ has outgoing edges with sender @p@ and receiver @q@,
--   @ready(v) = \{\{p,q\}\} ∪ ⋃ \{ pair ∈ ready(child) | pair ∩ \{p,q\} = ∅ \}@
--
-- * If @v@ has no outgoing edges, @ready(v) = ∅@
--
-- A graph is weak balanced iff @⋃ ready(v) ⊆ unavoidable(v)@ for every
-- reachable node @v@, where @unavoidable@ is the @must@ set from the
-- standard balancedness check.
checkWeakBalanced :: GlobalGraph -> BalancedResult
checkWeakBalanced gg =
  case violations of
    [] -> Right ()
    errs -> Left errs
  where
    reachable = reachableVertices gg
    own = ownParticipants gg reachable
    children = reachableChildren gg reachable

    -- Compute must (unavoidable) participants
    must0 = Map.fromSet (\v -> Map.findWithDefault Set.empty v own) reachable
    may0 = must0
    (mustFinal, _) = iterateToFixpoint own children must0 may0

    -- Compute ready pairs via fixpoint
    ownPrs = ownParticipantPairs gg reachable
    ready0 = Map.fromSet (const Set.empty) reachable
    readyFinal = readyFixpoint own ownPrs children ready0

    -- Check: flatten(ready(v)) ⊆ must(v) for all reachable v
    violations =
      [ UnbalancedVertex v mustSet readyPts
      | v <- Set.toList reachable
      , let mustSet = Map.findWithDefault Set.empty v mustFinal
      , let readyPts = flattenPairs (Map.findWithDefault Set.empty v readyFinal)
      , not (readyPts `Set.isSubsetOf` mustSet)
      ]

-- | For each reachable vertex, collect the set of @{sender, receiver}@ pairs
-- from its outgoing edges.
ownParticipantPairs ::
  GlobalGraph ->
  Set.Set G.Vertex ->
  Map.Map G.Vertex PairSet
ownParticipantPairs gg reachable =
  foldl' addPayloadPairs
    (foldl' addEdgePairs initial (Map.toList (ggEdgeLabels gg)))
    (Map.toList (ggPayloadEdges gg))
  where
    initial = Map.fromSet (const Set.empty) reachable

    addEdgePairs acc ((from, _), labels)
      | from `Set.member` reachable =
          let pairs = Set.fromList
                [ Set.fromList [geSender lbl, geReceiver lbl]
                | lbl <- labels
                ]
           in Map.insertWith Set.union from pairs acc
      | otherwise = acc

    addPayloadPairs acc ((from, _), labels)
      | from `Set.member` reachable =
          let pairs = Set.fromList
                [ Set.fromList [gpeSender lbl, gpeReceiver lbl]
                | lbl <- labels
                ]
           in Map.insertWith Set.union from pairs acc
      | otherwise = acc

-- | Flatten a set of participant pairs into the set of all participants.
flattenPairs :: PairSet -> ParticipantSet
flattenPairs = Set.foldl' Set.union Set.empty

-- | Compute ready sets to a fixpoint.
readyFixpoint ::
  Map.Map G.Vertex ParticipantSet ->
  Map.Map G.Vertex PairSet ->
  Map.Map G.Vertex (Set.Set G.Vertex) ->
  Map.Map G.Vertex PairSet ->
  Map.Map G.Vertex PairSet
readyFixpoint own ownPrs children ready =
  if ready == ready'
    then ready
    else readyFixpoint own ownPrs children ready'
  where
    ready' = Map.mapWithKey updateReady ready

    updateReady v _ =
      let ownPairV = Map.findWithDefault Set.empty v ownPrs
          ownPtsV = Map.findWithDefault Set.empty v own
          childPairs = foldl' Set.union Set.empty
            [ Set.filter (\pair -> Set.null (Set.intersection pair ownPtsV))
                (Map.findWithDefault Set.empty child ready)
            | child <- Set.toList (Map.findWithDefault Set.empty v children)
            ]
       in Set.union ownPairV childPairs
