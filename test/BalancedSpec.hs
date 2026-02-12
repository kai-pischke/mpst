module BalancedSpec (spec) where

import Automata
  ( GlobalEdgeLabel(..)
  , GlobalGraph(..)
  , GlobalNode(..)
  , RecVarHints(..)
  , buildGlobalGraph
  )
import Balanced (checkBalanced)
import Data.Array (array, assocs, bounds)
import Data.Either (isLeft)
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import Syntax (Label(..), Participant(..), parseGlobalTypeChecked)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "1) Balanced checking" $ do
    it "[BAL-001] rejects non-balanced recursive branch participation" $
      expectUnbalanced "rec t . p -> q {l1: t, l2: q -> r {l3: end}}"
    it "[BAL-002] accepts balanced recursive branch participation" $
      expectBalanced "rec t . p -> q {l1: q -> r {l4: t}, l2: q -> r {l3: end}}"
    it "[BAL-003] handles guarded recursion fixed points" $
      expectBalanced "rec t . p -> q {loop: t}"
    it "[BAL-004] ignores unreachable nodes from the start state" $
      expectBalancedGraph (injectUnreachableUnbalancedNode baseBalanced)

expectBalanced :: String -> Expectation
expectBalanced source =
  case parseAsGlobalGraph source of
    Left err -> expectationFailure err
    Right gg -> expectBalancedGraph gg

expectBalancedGraph :: GlobalGraph -> Expectation
expectBalancedGraph gg =
  checkBalanced gg `shouldBe` Right ()

expectUnbalanced :: String -> Expectation
expectUnbalanced source =
  case parseAsGlobalGraph source of
    Left err -> expectationFailure err
    Right gg ->
      checkBalanced gg `shouldSatisfy` isLeft

parseAsGlobalGraph :: String -> Either String GlobalGraph
parseAsGlobalGraph source =
  case parseGlobalTypeChecked source of
    Left err ->
      Left ("Input should parse and be well-formed, but failed with:\n" ++ err)
    Right g ->
      Right (buildGlobalGraph g)

baseBalanced :: GlobalGraph
baseBalanced =
  case parseAsGlobalGraph "p -> q {l: end}" of
    Left err -> error err
    Right gg -> gg

injectUnreachableUnbalancedNode :: GlobalGraph -> GlobalGraph
injectUnreachableUnbalancedNode gg =
  gg
    { ggGraph = newGraph
    , ggNodes = newNodes
    , ggEdgeLabels = newEdgeLabels
    }
  where
    (_, hi) = bounds (ggNodes gg)
    u = hi + 1
    v = hi + 2
    p = Participant "p"
    q = Participant "q"
    r = Participant "r"
    emptyHints = RecVarHints [] Nothing

    endVertex =
      case [vertex | (vertex, node) <- assocs (ggNodes gg), node == GlobalEndNode] of
        e : _ -> e
        [] -> error "Expected at least one end vertex in base graph."

    oldEdges = G.edges (ggGraph gg)
    addedEdges = [(u, endVertex), (u, v), (v, endVertex)]
    newGraph = G.buildG (0, v) (oldEdges ++ addedEdges)

    newNodes =
      array
        (0, v)
        ( assocs (ggNodes gg)
            ++ [ (u, GlobalNode)
               , (v, GlobalNode)
               ]
        )

    addedLabels =
      Map.fromList
        [ ( (u, endVertex)
          , [GlobalEdgeLabel p q (Label "u_end") emptyHints]
          )
        , ( (u, v)
          , [GlobalEdgeLabel p q (Label "u_step") emptyHints]
          )
        , ( (v, endVertex)
          , [GlobalEdgeLabel q r (Label "v_end") emptyHints]
          )
        ]

    newEdgeLabels = Map.union (ggEdgeLabels gg) addedLabels

