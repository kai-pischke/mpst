module LivenessSpec (spec) where

import Automata
  ( ContextEdgeLabel(..)
  , ContextGraph(..)
  , ContextState(..)
  , buildContextGraph
  , buildLocalGraph
  )
import Data.Array (array)
import Data.Either (fromRight, isLeft)
import qualified Data.Graph as G
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Liveness (SyncPath(..), allSyncPaths, checkLiveness, filterFairSyncPaths)
import Syntax (Label(..), Participant(..), parseLocalTypeChecked)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "4) Liveness checking" $ do
    it "[LIVE-PATH-001] enumerates finite sync-terminal paths" $
      allSyncPaths finiteGraph
        `shouldBe` [FiniteSyncPath (0 NE.:| [1])]

    it "[LIVE-PATH-002] enumerates lasso paths for sync loops" $
      allSyncPaths loopGraph
        `shouldBe` [LassoSyncPath [] (0 NE.:| [])]

    it "[LIVE-PATH-003] enumerates both finite and lasso paths when both exist" $
      Set.fromList (allSyncPaths mixedGraph)
        `shouldBe` Set.fromList
          [ FiniteSyncPath (0 NE.:| [1])
          , LassoSyncPath [0] (2 NE.:| [])
          ]
    it "[LIVE-PATH-004] enumerates the two paths for the recursive 2-party context example" $
      syncTracesOfContext
        [ ("p", "rec t . q ? {l1: t, l2: end}")
        , ("q", "rec t . p ! {l1: t, l2: end}")
        ]
        `shouldBe` Set.fromList
          [ FiniteTrace ["l2"]
          , LassoTrace [] ["l1"]
          ]
    it "[LIVE-PATH-005] enumerates finite and lasso paths in a larger 3-party context" $
      syncTracesOfContext
        [ ("p", "rec t . q ! {a: r ! {b: t}, c: end}")
        , ("q", "rec t . p ? {a: t, c: end}")
        , ("r", "rec t . p ? {b: t}")
        ]
        `shouldBe` Set.fromList
          [ FiniteTrace ["c"]
          , LassoTrace [] ["a", "b"]
          ]
    it "[LIVE-PATH-006] enumerates multiple distinct lassos from one context" $
      syncTracesOfContext
        [ ("p", "rec t . q ! {x: t, y: q ! {z: t}}")
        , ("q", "rec t . p ? {x: t, y: p ? {z: t}}")
        ]
        `shouldBe` Set.fromList
          [ LassoTrace [] ["x"]
          , LassoTrace [] ["y", "z"]
          ]
    it "[LIVE-PATH-007] 4 participants with independent loops admit interleaved lassos" $
      traces
        `shouldBe` expected
    it "[LIVE-FAIR-001] filters out unfair paths that starve enabled participant pairs" $
      fairTraces
        `shouldBe` fairExpected

    it "[LIVE-001] accepts a terminating protocol without deadlock" $
      expectLive
        [ ("p", "q ! {ok: end}")
        , ("q", "p ? {ok: end}")
        ]
    it "[LIVE-002] rejects a deadlocked communication cycle" $
      expectNotLive
        [ ("p", "rec t . q ! {a: t}")
        , ("q", "rec t . p ! {a: t}")
        ]
    it "[LIVE-003] rejects when a participant is waiting forever" $
      expectNotLive
        [ ("p", "rec t . q ? {a: t}")
        , ("q", "rec t . r ! {b: t}")
        , ("r", "rec t . q ? {b: t}")
        ]
    it "[LIVE-003] rejects when a participant is waiting forever" $
      expectNotLive
        [ ("p", "q ? {a: end}")
        , ("q", "rec t . r ! {a: p ! {a : end}, b: t}")
        , ("r", "rec t . q ? {b: t, a: end}")
        ]
    it "[LIVE-004] accepts productive recursion that can always advance" $
      expectLive
        [ ("p", "rec t . q ! {a: t}")
        , ("q", "rec t . p ? {a: t}")
        ]
  where
    traces =
      syncTracesOfContext
        [ ("p", "rec t . q ! {a: q ! {a2: t}}")
        , ("q", "rec t . p ? {a: p ? {a2: t}}")
        , ("r", "rec u . s ! {b: s ! {b2: u}}")
        , ("s", "rec u . r ? {b: r ? {b2: u}}")
        ]

    expected =
      Set.fromList
        [ LassoTrace [] ["a", "a2"]
        , LassoTrace [] ["b", "b2"]
        , LassoTrace [] ["a", "b", "a2", "b2"]
        , LassoTrace [] ["b", "a", "b2", "a2"]
        , LassoTrace ["a"] ["b","b2"]
        , LassoTrace ["a","b"] ["a2","a"]
        , LassoTrace ["b"] ["a","a2"]
        , LassoTrace ["b","a"] ["b2","b"]
        ]

    fairTraces =
      syncTracesOfPaths independentLoopsGraph (filterFairSyncPaths independentLoopsGraph (allSyncPaths independentLoopsGraph))

    fairExpected =
      Set.fromList
        [ LassoTrace [] ["a", "b", "a2", "b2"]
        , LassoTrace [] ["b", "a", "b2", "a2"]
        ]

    independentLoopsGraph =
      contextGraphOf
        [ ("p", "rec t . q ! {a: q ! {a2: t}}")
        , ("q", "rec t . p ? {a: p ? {a2: t}}")
        , ("r", "rec u . s ! {b: s ! {b2: u}}")
        , ("s", "rec u . r ? {b: r ? {b2: u}}")
        ]

finiteGraph :: ContextGraph
finiteGraph =
  mkContextGraph
    2
    [ (0, 1, [sync "p" "q" "l"])
    ]

loopGraph :: ContextGraph
loopGraph =
  mkContextGraph
    1
    [ (0, 0, [sync "p" "q" "l"])
    ]

mixedGraph :: ContextGraph
mixedGraph =
  mkContextGraph
    3
    [ (0, 1, [sync "p" "q" "a"])
    , (0, 2, [sync "p" "q" "b"])
    , (2, 2, [sync "p" "q" "b"])
    ]

mkContextGraph :: Int -> [(Int, Int, [ContextEdgeLabel])] -> ContextGraph
mkContextGraph n transitions =
  ContextGraph
    { cgGraph = G.buildG (0, n - 1) edgePairs
    , cgStart = 0
    , cgNodes = array (0, n - 1) [(v, ContextState Map.empty) | v <- [0 .. n - 1]]
    , cgParticipants = [Participant "p", Participant "q"]
    , cgEdgeLabels = Map.fromList [((from, to), labels) | (from, to, labels) <- transitions]
    }
  where
    edgePairs = [(from, to) | (from, to, _) <- transitions]

sync :: String -> String -> String -> ContextEdgeLabel
sync sender receiver lbl =
  ContextSyncEdge
    { ceSender = Participant sender
    , ceReceiver = Participant receiver
    , ceLabel = Label lbl
    }

data SyncTrace
  = FiniteTrace [String]
  | LassoTrace [String] [String]
  deriving (Eq, Ord, Show)

syncTracesOfContext :: [(String, String)] -> Set.Set SyncTrace
syncTracesOfContext participantLocals =
  syncTracesOfPaths cg (allSyncPaths cg)
  where
    cg = contextGraphOf participantLocals

syncTracesOfPaths :: ContextGraph -> [SyncPath] -> Set.Set SyncTrace
syncTracesOfPaths cg paths =
  Set.fromList (map (pathTrace cg) paths)

contextGraphOf :: [(String, String)] -> ContextGraph
contextGraphOf participantLocals =
  fromRight
    (error "Invalid local type in liveness test fixture.")
    (parseAsContextGraph participantLocals)

pathTrace :: ContextGraph -> SyncPath -> SyncTrace
pathTrace cg path =
  case path of
    FiniteSyncPath states ->
      FiniteTrace (map edgeLabel (stateEdges (NE.toList states)))
    LassoSyncPath stem cycleStates ->
      let cycleList = NE.toList cycleStates
          stemEdgePairs = stateEdgesWithEntry stem cycleList
          cycleEdgePairs = zip cycleList (tail cycleList ++ [head cycleList])
       in LassoTrace
            (map edgeLabel stemEdgePairs)
            (map edgeLabel cycleEdgePairs)
  where
    edgeLabel (from, to) =
      case syncLabelsBetween cg from to of
        [lbl] -> lbl
        [] ->
          error
            ( "Expected sync label on edge "
                ++ show (from, to)
                ++ " while constructing path trace."
            )
        lbls ->
          error
            ( "Expected unique sync label on edge "
                ++ show (from, to)
                ++ ", found: "
                ++ show lbls
            )

stateEdges :: [G.Vertex] -> [(G.Vertex, G.Vertex)]
stateEdges states = zip states (drop 1 states)

stateEdgesWithEntry :: [G.Vertex] -> [G.Vertex] -> [(G.Vertex, G.Vertex)]
stateEdgesWithEntry stem cycleList =
  case stem of
    [] -> []
    _ ->
      zip stem (drop 1 stem ++ [head cycleList])

syncLabelsBetween :: ContextGraph -> G.Vertex -> G.Vertex -> [String]
syncLabelsBetween cg from to =
  [ lbl
  | ContextSyncEdge _ _ (Label lbl) <- Map.findWithDefault [] (from, to) (cgEdgeLabels cg)
  ]

expectLive :: [(String, String)] -> IO ()
expectLive participantLocals =
  checkLiveness (contextGraphOf participantLocals) `shouldBe` Right ()

expectNotLive :: [(String, String)] -> IO ()
expectNotLive participantLocals =
  checkLiveness (contextGraphOf participantLocals) `shouldSatisfy` isLeft

parseAsContextGraph :: [(String, String)] -> Either String ContextGraph
parseAsContextGraph participantLocals = do
  locals <- mapM parseOne participantLocals
  pure (buildContextGraph locals)
  where
    parseOne (participantName, localSrc) =
      case parseLocalTypeChecked localSrc of
        Left err ->
          Left
            ( "Local type for participant "
                ++ show participantName
                ++ " failed to parse/check:\n"
                ++ err
            )
        Right lt ->
          Right (Participant participantName, buildLocalGraph lt)
