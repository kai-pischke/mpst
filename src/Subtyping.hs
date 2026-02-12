-- | Subtyping checks for local graphs.
--
-- The check computes the greatest simulation relation by fixpoint pruning.
module Subtyping
  ( SubtypingError(..)
  , SubtypingResult
  , checkLocalSubtype
  ) where

import Automata
  ( LocalDirection(..)
  , LocalEdgeLabel(..)
  , LocalGraph(..)
  , LocalNode(..)
  )
import Data.Array (assocs)
import Data.Foldable (foldl')
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax.AST (Label, Participant)

type StatePair = (G.Vertex, G.Vertex)
type Simulation = Set.Set StatePair

data LocalTransition = LocalTransition
  { ltTo :: !G.Vertex
  , ltDirection :: LocalDirection
  , ltPeer :: Participant
  , ltLabel :: Label
  }
  deriving (Eq, Ord, Show)

-- | Errors reported by local subtyping.
data SubtypingError
  = StartStatesNotInSimulation
      { stLeftStart :: !G.Vertex
      , stRightStart :: !G.Vertex
      }
  deriving (Eq, Show)

-- | Result of local subtyping.
type SubtypingResult = Either [SubtypingError] ()

-- | Check whether the left local graph is a subtype of the right local graph.
--
-- The relation is computed by repeatedly removing state-pairs that violate:
--
-- * receive (left): right-receive transitions must be simulated (contravariant)
-- * send (left): left-send transitions must be simulated by right (covariant)
checkLocalSubtype :: LocalGraph -> LocalGraph -> SubtypingResult
checkLocalSubtype left right =
  if (lgStart left, lgStart right) `Set.member` fixed
    then Right ()
    else Left [StartStatesNotInSimulation (lgStart left) (lgStart right)]
  where
    leftNodes = Map.fromList (assocs (lgNodes left))
    rightNodes = Map.fromList (assocs (lgNodes right))
    leftOutgoing = collectOutgoing (lgEdgeLabels left)
    rightOutgoing = collectOutgoing (lgEdgeLabels right)
    initial = initialSimulation leftNodes rightNodes
    fixed = pruneToFixpoint leftNodes rightNodes leftOutgoing rightOutgoing initial

initialSimulation ::
  Map.Map G.Vertex LocalNode ->
  Map.Map G.Vertex LocalNode ->
  Simulation
initialSimulation leftNodes rightNodes =
  Set.fromList
    [ (l, r)
    | (l, ln) <- Map.toList leftNodes
    , (r, rn) <- Map.toList rightNodes
    , compatibleNodeKinds ln rn
    ]

compatibleNodeKinds :: LocalNode -> LocalNode -> Bool
compatibleNodeKinds leftNode rightNode =
  case (leftNode, rightNode) of
    (LocalEndNode, LocalEndNode) -> True
    (LocalSendNode p _, LocalSendNode q _) -> p == q
    (LocalRecvNode p _, LocalRecvNode q _) -> p == q
    _ -> False

pruneToFixpoint ::
  Map.Map G.Vertex LocalNode ->
  Map.Map G.Vertex LocalNode ->
  Map.Map G.Vertex [LocalTransition] ->
  Map.Map G.Vertex [LocalTransition] ->
  Simulation ->
  Simulation
pruneToFixpoint leftNodes rightNodes leftOutgoing rightOutgoing =
  go
  where
    go sim =
      let bad =
            Set.filter
              (violatesSimulation leftNodes rightNodes leftOutgoing rightOutgoing sim)
              sim
          sim' = Set.difference sim bad
       in if Set.null bad then sim else go sim'

violatesSimulation ::
  Map.Map G.Vertex LocalNode ->
  Map.Map G.Vertex LocalNode ->
  Map.Map G.Vertex [LocalTransition] ->
  Map.Map G.Vertex [LocalTransition] ->
  Simulation ->
  StatePair ->
  Bool
violatesSimulation leftNodes rightNodes leftOutgoing rightOutgoing sim (leftV, rightV) =
  case (Map.lookup leftV leftNodes, Map.lookup rightV rightNodes) of
    (Just LocalEndNode, Just LocalEndNode) ->
      False
    (Just (LocalSendNode _ _), Just (LocalSendNode _ _)) ->
      not (allRequiredSimulated leftSends rightSends)
    (Just (LocalRecvNode _ _), Just (LocalRecvNode _ _)) ->
      not (allRequiredSimulated rightRecvs leftRecvs)
    _ ->
      True
  where
    leftOutgoingAt = Map.findWithDefault [] leftV leftOutgoing
    rightOutgoingAt = Map.findWithDefault [] rightV rightOutgoing
    leftSends = filter (\t -> ltDirection t == Send) leftOutgoingAt
    rightSends = filter (\t -> ltDirection t == Send) rightOutgoingAt
    leftRecvs = filter (\t -> ltDirection t == Receive) leftOutgoingAt
    rightRecvs = filter (\t -> ltDirection t == Receive) rightOutgoingAt

    allRequiredSimulated required available =
      all (hasMatchingSuccessor available) required

    hasMatchingSuccessor available required =
      any
        (\candidate -> sameAction required candidate && (ltTo required, ltTo candidate) `Set.member` sim)
        available

sameAction :: LocalTransition -> LocalTransition -> Bool
sameAction lhs rhs =
  ltDirection lhs == ltDirection rhs
    && ltPeer lhs == ltPeer rhs
    && ltLabel lhs == ltLabel rhs

collectOutgoing ::
  Map.Map G.Edge [LocalEdgeLabel] ->
  Map.Map G.Vertex [LocalTransition]
collectOutgoing =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (addTransition from to) acc labels)
    Map.empty
  where
    addTransition from to acc label =
      Map.insertWith
        (++)
        from
        [ LocalTransition
            { ltTo = to
            , ltDirection = leDirection label
            , ltPeer = lePeer label
            , ltLabel = leLabel label
            }
        ]
        acc
