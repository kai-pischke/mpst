module Automata
  ( GlobalGraph(..)
  , GlobalNode(..)
  , GlobalEdgeLabel(..)
  , buildGlobalGraph
  , LocalGraph(..)
  , LocalNode(..)
  , LocalDirection(..)
  , LocalEdgeLabel(..)
  , buildLocalGraph
  ) where

import Control.Monad.Fix (mfix)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.Array (array)
import Data.Foldable (for_)
import qualified Data.Graph as G
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as Env
import qualified Data.Map.Strict as Map
import Syntax.AST

-- | Internal state for incrementally building graphs.
data GraphBuilder node edge = GraphBuilder
  { gbNext :: !G.Vertex
  , gbNodes :: Map.Map G.Vertex node
  , gbEdges :: [(G.Edge, edge)]
  }

emptyBuilder :: GraphBuilder node edge
emptyBuilder = GraphBuilder 0 Map.empty []

freshNode :: node -> State (GraphBuilder node edge) G.Vertex
freshNode label = do
  v <- gets gbNext
  modify $ \s ->
    s
      { gbNext = v + 1
      , gbNodes = Map.insert v label (gbNodes s)
      }
  pure v

addEdge :: G.Vertex -> G.Vertex -> edge -> State (GraphBuilder node edge) ()
addEdge from to label =
  modify $ \s -> s {gbEdges = ((from, to), label) : gbEdges s}

lookupVar :: Env.Map TypeVar G.Vertex -> TypeVar -> State (GraphBuilder node edge) G.Vertex
lookupVar env var =
  case Env.lookup var env of
    Just v -> pure v
    Nothing -> error ("Unbound recursion variable: " <> show (getTypeVar var))

graphBounds :: GraphBuilder node edge -> (G.Vertex, G.Vertex)
graphBounds builder
  | gbNext builder <= 0 = error "Automata: no vertices generated"
  | otherwise = (0, gbNext builder - 1)

-- Global graphs

data GlobalNode
  = GlobalNode
  | GlobalEndNode
  deriving (Eq, Show)

data GlobalEdgeLabel = GlobalEdgeLabel
  { geSender :: Participant
  , geReceiver :: Participant
  , geLabel :: Label
  }
  deriving (Eq, Ord, Show)

data GlobalGraph = GlobalGraph
  { ggGraph :: G.Graph
  , ggStart :: G.Vertex
  , ggNodes :: G.Table GlobalNode
  , ggEdgeLabels :: Map.Map G.Edge [GlobalEdgeLabel]
  }
  deriving (Eq, Show)

buildGlobalGraph :: GlobalType -> GlobalGraph
buildGlobalGraph gType =
  let (start, builder) = runState (globalNode Env.empty gType) emptyBuilder
   in finaliseGlobal start builder

globalNode :: Env.Map TypeVar G.Vertex -> GlobalType -> State (GraphBuilder GlobalNode GlobalEdgeLabel) G.Vertex
globalNode env gtype = case gtype of
  GMessage sender receiver branches -> do
    v <- freshNode GlobalNode
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- globalNode env cont
      addEdge v dest (GlobalEdgeLabel sender receiver lbl)
    pure v
  GVar var -> lookupVar env var
  GRec var body ->
    mfix $ \start -> globalNode (Env.insert var start env) body
  GEnd -> freshNode GlobalEndNode

finaliseGlobal :: G.Vertex -> GraphBuilder GlobalNode GlobalEdgeLabel -> GlobalGraph
finaliseGlobal start builder =
  let bounds = graphBounds builder
      graph = G.buildG bounds (map fst (gbEdges builder))
      nodeTable = array bounds (Map.toList (gbNodes builder))
      edgeLabels = collectEdges (gbEdges builder)
   in GlobalGraph
        { ggGraph = graph
        , ggStart = start
        , ggNodes = nodeTable
        , ggEdgeLabels = edgeLabels
        }

-- Local graphs

data LocalDirection = Send | Receive
  deriving (Eq, Ord, Show)

data LocalEdgeLabel = LocalEdgeLabel
  { leDirection :: LocalDirection
  , lePeer :: Participant
  , leLabel :: Label
  }
  deriving (Eq, Ord, Show)

data LocalNode
  = LocalSendNode Participant [Label]
  | LocalRecvNode Participant [Label]
  | LocalEndNode
  deriving (Eq, Show)

data LocalGraph = LocalGraph
  { lgGraph :: G.Graph
  , lgStart :: G.Vertex
  , lgNodes :: G.Table LocalNode
  , lgEdgeLabels :: Map.Map G.Edge [LocalEdgeLabel]
  }
  deriving (Eq, Show)

buildLocalGraph :: LocalType -> LocalGraph
buildLocalGraph lType =
  let (start, builder) = runState (localNode Env.empty lType) emptyBuilder
   in finaliseLocal start builder

localNode :: Env.Map TypeVar G.Vertex -> LocalType -> State (GraphBuilder LocalNode LocalEdgeLabel) G.Vertex
localNode env lt = case lt of
  LSend peer branches -> do
    let labels = fmap fst (NE.toList branches)
    v <- freshNode (LocalSendNode peer labels)
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- localNode env cont
      addEdge v dest (LocalEdgeLabel Send peer lbl)
    pure v
  LRecv peer branches -> do
    let labels = fmap fst (NE.toList branches)
    v <- freshNode (LocalRecvNode peer labels)
    for_ (NE.toList branches) $ \(lbl, cont) -> do
      dest <- localNode env cont
      addEdge v dest (LocalEdgeLabel Receive peer lbl)
    pure v
  LVar var -> lookupVar env var
  LRec var body ->
    mfix $ \start -> localNode (Env.insert var start env) body
  LEnd -> freshNode LocalEndNode

finaliseLocal :: G.Vertex -> GraphBuilder LocalNode LocalEdgeLabel -> LocalGraph
finaliseLocal start builder =
  let bounds = graphBounds builder
      graph = G.buildG bounds (map fst (gbEdges builder))
      nodeTable = array bounds (Map.toList (gbNodes builder))
      edgeLabels = collectEdges (gbEdges builder)
   in LocalGraph
        { lgGraph = graph
        , lgStart = start
        , lgNodes = nodeTable
        , lgEdgeLabels = edgeLabels
        }

collectEdges :: Ord k => [(k, v)] -> Map.Map k [v]
collectEdges = foldr step Map.empty
  where
    step (k, v) acc = Map.insertWith (++) k [v] acc
