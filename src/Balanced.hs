module Balanced where
---  ( BalancedError(..)
--  , BalancedResult
--  , checkBalanced
--  ) where
{-
import Automata (GlobalEdgeLabel(..), GlobalGraph(..))
import Data.Foldable (foldl')
import Data.List (foldl1')
import qualified Data.Graph as G
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST (Participant)

type ParticipantSet = Set.Set Participant
type ParticipantMap = IntMap.IntMap ParticipantSet
type OutEdges = IntMap.IntMap [(G.Vertex, ParticipantSet)]

data BalancedError = BalancedMismatch
  { bmVertex :: !G.Vertex
  , bmMay :: ParticipantSet
  , bmMust :: ParticipantSet
  }
  deriving (Eq, Show)

type BalancedResult = Either [BalancedError] ()

data BalancedState = BalancedState
  { bsMay :: ParticipantMap
  , bsMust :: ParticipantMap
  }
  deriving (Eq, Show)

-- | A global graph is balanced when, for every reachable node, the participants
-- that may appear (union of outgoing edges) and the participants that must
-- appear (intersection of outgoing edges) coincide.
checkBalanced :: GlobalGraph -> BalancedResult
checkBalanced gg =
  let reach = Set.fromList $ G.reachable (ggGraph gg) (ggStart gg)
      outgoing = outgoingParticipants gg reach
      universe = allParticipants outgoing
      BalancedState maySets mustSets = solve outgoing reachableVerts universe
      mismatches =
        [ BalancedMismatch v (maySets IntMap.! v) (mustSets IntMap.! v)
        | v <- Set.toList reach
        , maySets IntMap.! v /= mustSets IntMap.! v
        ]
   in if null mismatches then Right () else Left mismatches

-- | Collect participant sets for each outgoing edge (restricted to reachable nodes).
outgoingParticipants :: GlobalGraph -> Set.Set G.Vertex -> OutEdges
outgoingParticipants gg reachable =
  Map.foldlWithKey' step IntMap.empty (ggEdgeLabels gg)
  where
    step acc (from, to) labels
      | from `Set.member` reachable && to `Set.member` reachable =
          let ps = foldl' addParticipants Set.empty labels
              addParticipants s (GlobalEdgeLabel s' r' _) = Set.insert s' (Set.insert r' s)
           in IntMap.insertWith (++) from [(to, ps)] acc
      | otherwise = acc

-- | All participants that show up on any outgoing edge.
allParticipants :: OutEdges -> ParticipantSet
allParticipants =
  IntMap.foldl'
    (\acc outs -> acc `Set.union` foldl' (\s (_, ps) -> s `Set.union` ps) Set.empty outs)
    Set.empty

-- | Iterate may/must equations until a fixed point is reached.
solve :: OutEdges -> [G.Vertex] -> ParticipantSet -> BalancedState
solve outgoing verts universe = go initial
  where
    initial =
      BalancedState
        { bsMay = IntMap.fromList [(v, Set.empty) | v <- verts]
        , bsMust = IntMap.fromList [(v, universe) | v <- verts]
        }

    go st =
      let st' = step st
       in if st' == st then st else go st'

    step (BalancedState may must) =
      let may' = IntMap.fromList [(v, mayValue v) | v <- verts]
          must' = IntMap.fromList [(v, mustValue v) | v <- verts]
       in BalancedState may' must'
      where
        mayValue v =
          foldl'
            (\s (w, ps) -> s `Set.union` ps `Set.union` lookupMay w)
            Set.empty
            (outs v)
        mustValue v =
          case outs v of
            [] -> Set.empty
            es ->
              foldl1'
                Set.intersection
                [ ps `Set.union` lookupMust w | (w, ps) <- es ]

        lookupMay v = IntMap.findWithDefault Set.empty v may
        lookupMust v = IntMap.findWithDefault universe v must
        outs v = IntMap.findWithDefault [] v outgoing
 -}