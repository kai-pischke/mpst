-- | Projection algorithms from global graphs to participant-local graphs.
module Project
  ( projectCoinductiveFull
  , projectCoinductivePlain
  , projectInductiveFull
  , projectInductivePlain
  , ProjectionError(..)
  , ProjectionResult
  ) where

import Automata
  ( GlobalEdgeLabel(..)
  , GlobalGraph(..)
  , GlobalNode(..)
  , LocalDirection(..)
  , LocalEdgeLabel(..)
  , LocalGraph(..)
  , LocalNode(..)
  , RecVarHints(..)
  )
import Control.Monad (foldM, unless)
import Control.Monad.State.Strict (gets, modify)
import Control.Monad.Trans.State.Strict (StateT(..), runStateT)
import Data.Array (assocs, array)
import Data.Foldable (foldl')
import Data.List (sortOn)
import qualified Data.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Merge (Merge, fullMerge, plainMerge)
import Syntax.AST (Label, Participant, TypeVar)

-- | Projection-specific failure.
data ProjectionError = ProjectionError String
  deriving (Eq, Show)

-- | Result type returned by projection algorithms.
type ProjectionResult = Either ProjectionError LocalGraph

projectCoinductiveFull, projectCoinductivePlain, projectInductiveFull :: GlobalGraph -> Participant -> ProjectionResult
projectCoinductiveFull _ _ = Left (ProjectionError "projectCoinductiveFull not implemented yet")
projectCoinductivePlain _ _ = Left (ProjectionError "projectCoinductivePlain not implemented yet")
projectInductiveFull = projectInductiveWith True fullMerge

-- | Inductive/plain projection is inductive projection parameterised by
-- plain merge (isomorphic branches only).
projectInductivePlain :: GlobalGraph -> Participant -> ProjectionResult
projectInductivePlain = projectInductiveWith False plainMerge

-- | Generic inductive projection parameterised by a branch-merge operator.
projectInductiveWith :: Bool -> Merge -> GlobalGraph -> Participant -> ProjectionResult
projectInductiveWith allowRecvUnion mergeFn gg p = do
  let env =
        ProjEnv
          { peParticipant = p
          , peAllowRecvUnion = allowRecvUnion
          , peMerge = mergeFn
          , peGlobalNodes = Map.fromList (assocs (ggNodes gg))
          , peOutgoing = globalOutgoing (ggEdgeLabels gg)
          }
      st0 = ProjState 1 Map.empty emptyBuildGraph
  (target, st) <- runStateT (projectAt env Set.empty (ggStartVarHints gg) (ggStart gg) 0) st0
  materialiseLocalGraph (psBuild st) target

data ProjectionAction = ProjectSend Participant | ProjectReceive Participant
  deriving (Eq, Show)

data ProjectionTarget = ProjectionTarget
  { ptVertex :: !G.Vertex
  , ptHints :: RecVarHints
  } deriving (Eq, Show)

data BuildGraph = BuildGraph
  { bgNodes :: Map.Map G.Vertex LocalNode
  , bgEdges :: [((G.Vertex, G.Vertex), LocalEdgeLabel)]
  }

emptyBuildGraph :: BuildGraph
emptyBuildGraph = BuildGraph Map.empty []

data ProjEnv = ProjEnv
  { peParticipant :: Participant
  , peAllowRecvUnion :: Bool
  , peMerge :: Merge
  , peGlobalNodes :: Map.Map G.Vertex GlobalNode
  , peOutgoing :: Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)]
  }

data ProjState = ProjState
  { psNextFresh :: !G.Vertex
  , psGlobalToLocal :: Map.Map G.Vertex G.Vertex
  , psBuild :: BuildGraph
  }

type ProjM a = StateT ProjState (Either ProjectionError) a

-- | DFS projection from global vertex @gv@ into local start @lv@.
projectAt :: ProjEnv -> Set.Set G.Vertex -> RecVarHints -> G.Vertex -> G.Vertex -> ProjM ProjectionTarget
projectAt env ignored hints gv lv = do
  mapped <- gets (Map.lookup gv . psGlobalToLocal)
  case mapped of
    Just lv' -> pure (ProjectionTarget lv' hints)
    Nothing -> do
      modify (\s -> s {psGlobalToLocal = Map.insert gv lv (psGlobalToLocal s)})
      node <- liftEither $ lookupGlobalNode (peGlobalNodes env) gv
      case node of
        GlobalEndNode -> do
          ensureNodeM lv LocalEndNode
          pure (ProjectionTarget lv hints)
        GlobalNode -> do
          branches <- liftEither $ outgoingAt gv (peOutgoing env)
          action <- liftEither $ actionFor (peParticipant env) gv branches
          case action of
            Just a -> projectInvolved env hints lv a branches
            Nothing -> projectIgnored env ignored hints gv lv branches

projectInvolved :: ProjEnv -> RecVarHints -> G.Vertex -> ProjectionAction -> [(GlobalEdgeLabel, G.Vertex)] -> ProjM ProjectionTarget
projectInvolved env hints lv action branches = do
  let sorted = sortOn (\(lbl, dst) -> (geLabel lbl, dst)) branches
      labels = fmap (geLabel . fst) sorted
      node =
        case action of
          ProjectSend peer -> LocalSendNode peer labels
          ProjectReceive peer -> LocalRecvNode peer labels
  ensureChoiceNodeM (peAllowRecvUnion env) lv node
  mapM_ step sorted
  pure (ProjectionTarget lv hints)
  where
    step (edgeLbl, child) = do
      existing <-
        gets
          ( existingEdgeTarget
              (actionDirection action)
              (actionPeer action)
              (geLabel edgeLbl)
              lv
              . psBuild
          )
      childStart <- maybe freshLocalVertex pure existing
      target <- projectAt env Set.empty (geTargetHints edgeLbl) child childStart
      let lbl =
            LocalEdgeLabel
              (actionDirection action)
              (actionPeer action)
              (geLabel edgeLbl)
              (ptHints target)
      insertEdgeM lv (ptVertex target) lbl

-- | Ignore a global node and merge all branch projections with 'peMerge'.
projectIgnored :: ProjEnv -> Set.Set G.Vertex -> RecVarHints -> G.Vertex -> G.Vertex -> [(GlobalEdgeLabel, G.Vertex)] -> ProjM ProjectionTarget
projectIgnored env ignored hints gv lv branches =
  if gv `Set.member` ignored
    then do
      -- Uninvolved cycle: cut branch and terminate locally.
      ensureNodeM lv LocalEndNode
      pure (ProjectionTarget lv hints)
    else do
      let sorted = sortOn (\(lbl, dst) -> (geLabel lbl, dst)) branches
      targets <-
        mapM
          (\(edgeLbl, child) ->
              projectAt
                env
                (Set.insert gv ignored)
                (appendHints hints (geTargetHints edgeLbl))
                child
                lv
          )
          sorted
      mergeIgnoredTargetsM env gv targets

mergeIgnoredTargetsM :: ProjEnv -> G.Vertex -> [ProjectionTarget] -> ProjM ProjectionTarget
mergeIgnoredTargetsM env gv targets =
  case targets of
    [] ->
      failProjection ("Invalid global graph: ignored vertex " ++ show gv ++ " has no outgoing branches.")
    [one] -> pure one
    firstT : rest -> do
      build <- gets psBuild
      firstG <- liftEither $ materialiseLocalGraph build firstT
      _ <- foldM (mergeOne build (ptHints firstT)) firstG rest
      pure firstT
  where
    mergeOne build expectedHints accGraph otherT = do
      unless (ptHints otherT == expectedHints) $
        failProjection
          ( "Inductive projection failed at ignored vertex "
              ++ show gv
              ++ ": branches carry different hint annotations."
          )
      otherG <- liftEither $ materialiseLocalGraph build otherT
      case peMerge env accGraph otherG of
        Nothing ->
          failProjection
            ( "Inductive projection failed at ignored vertex "
                ++ show gv
                ++ ": branch projections cannot be merged."
            )
        Just merged ->
          pure merged

actionFor :: Participant -> G.Vertex -> [(GlobalEdgeLabel, G.Vertex)] -> Either ProjectionError (Maybe ProjectionAction)
actionFor p v branches =
  case fmap (branchAction p . fst) branches of
    [] ->
      Left $ ProjectionError ("Invalid global graph: message vertex " ++ show v ++ " has no outgoing transitions.")
    acts
      | all (== Nothing) acts -> Right Nothing
      | otherwise ->
          case sequence acts of
            Just (a : as) | all (== a) as -> Right (Just a)
            Just _ ->
              Left
                ( ProjectionError
                    ( "Inductive projection failed: participant "
                        ++ show p
                        ++ " has inconsistent role at vertex "
                        ++ show v
                        ++ "."
                    )
                )
            Nothing ->
              Left
                ( ProjectionError
                    ( "Inductive projection failed: participant "
                        ++ show p
                        ++ " appears in only some branches at vertex "
                        ++ show v
                        ++ "."
                    )
                )

branchAction :: Participant -> GlobalEdgeLabel -> Maybe ProjectionAction
branchAction p e
  | geSender e == p = Just (ProjectSend (geReceiver e))
  | geReceiver e == p = Just (ProjectReceive (geSender e))
  | otherwise = Nothing

actionDirection :: ProjectionAction -> LocalDirection
actionDirection (ProjectSend _) = Send
actionDirection (ProjectReceive _) = Receive

actionPeer :: ProjectionAction -> Participant
actionPeer (ProjectSend peer) = peer
actionPeer (ProjectReceive peer) = peer

appendHints :: RecVarHints -> RecVarHints -> RecVarHints
appendHints left right =
  RecVarHints
    { rvhBinders = dedupeTypeVars (rvhBinders left ++ rvhBinders right)
    , rvhPreferredVar = case rvhPreferredVar right of
        Just tv -> Just tv
        Nothing -> rvhPreferredVar left
    }

dedupeTypeVars :: [TypeVar] -> [TypeVar]
dedupeTypeVars = reverse . fst . foldl' step ([], Set.empty)
  where
    step (acc, seen) tv
      | tv `Set.member` seen = (acc, seen)
      | otherwise = (tv : acc, Set.insert tv seen)

lookupGlobalNode :: Map.Map G.Vertex GlobalNode -> G.Vertex -> Either ProjectionError GlobalNode
lookupGlobalNode nodes v =
  maybe
    (Left (ProjectionError ("Global vertex " ++ show v ++ " has no node metadata.")))
    Right
    (Map.lookup v nodes)

outgoingAt :: G.Vertex -> Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)] -> Either ProjectionError [(GlobalEdgeLabel, G.Vertex)]
outgoingAt v out =
  maybe
    (Left (ProjectionError ("Invalid global graph: message vertex " ++ show v ++ " has no outgoing transitions.")))
    Right
    (Map.lookup v out)

globalOutgoing :: Map.Map G.Edge [GlobalEdgeLabel] -> Map.Map G.Vertex [(GlobalEdgeLabel, G.Vertex)]
globalOutgoing =
  Map.foldlWithKey'
    (\acc (from, to) labels -> foldl' (\m lbl -> Map.insertWith (++) from [(lbl, to)] m) acc labels)
    Map.empty

freshLocalVertex :: ProjM G.Vertex
freshLocalVertex = do
  v <- gets psNextFresh
  modify (\s -> s {psNextFresh = v + 1})
  pure v

ensureNodeM :: G.Vertex -> LocalNode -> ProjM ()
ensureNodeM v node = do
  build <- gets psBuild
  build' <- liftEither $ ensureNode v node build
  modify (\s -> s {psBuild = build'})

ensureChoiceNodeM :: Bool -> G.Vertex -> LocalNode -> ProjM ()
ensureChoiceNodeM allowRecvUnion v node = do
  build <- gets psBuild
  build' <- liftEither $ ensureChoiceNode allowRecvUnion v node build
  modify (\s -> s {psBuild = build'})

insertEdgeM :: G.Vertex -> G.Vertex -> LocalEdgeLabel -> ProjM ()
insertEdgeM from to lbl = do
  build <- gets psBuild
  build' <- liftEither $ insertEdge from to lbl build
  modify (\s -> s {psBuild = build'})

ensureNode :: G.Vertex -> LocalNode -> BuildGraph -> Either ProjectionError BuildGraph
ensureNode v node build =
  case Map.lookup v (bgNodes build) of
    Nothing -> Right build {bgNodes = Map.insert v node (bgNodes build)}
    Just same | same == node -> Right build
    Just _ ->
      Left
        ( ProjectionError
            ( "Projection conflict at local vertex "
                ++ show v
                ++ ": incompatible node metadata."
            )
        )

ensureChoiceNode :: Bool -> G.Vertex -> LocalNode -> BuildGraph -> Either ProjectionError BuildGraph
ensureChoiceNode allowRecvUnion v node build =
  case Map.lookup v (bgNodes build) of
    Nothing -> Right build {bgNodes = Map.insert v node (bgNodes build)}
    Just same
      | same == node -> Right build
      | allowRecvUnion ->
          case (same, node) of
            (LocalRecvNode peerOld labelsOld, LocalRecvNode peerNew labelsNew)
              | peerOld == peerNew ->
                  let merged = LocalRecvNode peerOld (Set.toAscList (Set.fromList labelsOld `Set.union` Set.fromList labelsNew))
                   in Right build {bgNodes = Map.insert v merged (bgNodes build)}
            _ ->
              projectionConflict
      | otherwise ->
          projectionConflict
  where
    projectionConflict =
      Left
        ( ProjectionError
            ( "Projection conflict at local vertex "
                ++ show v
                ++ ": incompatible node metadata."
            )
        )

-- | Insert branch edge. Same source+label must agree on target+metadata.
insertEdge :: G.Vertex -> G.Vertex -> LocalEdgeLabel -> BuildGraph -> Either ProjectionError BuildGraph
insertEdge from to lbl build =
  case matches of
    [] -> Right build {bgEdges = ((from, to), lbl) : bgEdges build}
    _ ->
      if any (\(to', lbl') -> to' == to && lbl' == lbl) matches
        then Right build
        else
          Left
            ( ProjectionError
                ( "Projection conflict at local vertex "
                    ++ show from
                    ++ ": duplicate branch label "
                    ++ show (leLabel lbl)
                    ++ " with incompatible targets."
                )
            )
  where
    matches =
      [ (to', lbl')
      | ((from', to'), lbl') <- bgEdges build
      , from' == from
      , leDirection lbl' == leDirection lbl
      , lePeer lbl' == lePeer lbl
      , leLabel lbl' == leLabel lbl
      ]

existingEdgeTarget :: LocalDirection -> Participant -> Label -> G.Vertex -> BuildGraph -> Maybe G.Vertex
existingEdgeTarget direction peer label from build =
  case matches of
    ((to, _) : _) -> Just to
    [] -> Nothing
  where
    matches =
      [ (to', lbl')
      | ((from', to'), lbl') <- bgEdges build
      , from' == from
      , leDirection lbl' == direction
      , lePeer lbl' == peer
      , leLabel lbl' == label
      ]

materialiseLocalGraph :: BuildGraph -> ProjectionTarget -> Either ProjectionError LocalGraph
materialiseLocalGraph build target = do
  _ <- requireNode oldStart (bgNodes build)
  let reachable = bfsReachable (localOutgoing (bgEdges build)) oldStart
      renaming = Map.fromList (zip reachable [0 ..])
      n = length reachable
  unless (n > 0) $
    Left (ProjectionError "Projection produced an empty local graph.")
  nodes <-
    mapM
      (\oldV -> do
          node <- requireNode oldV (bgNodes build)
          pure (renaming Map.! oldV, node)
      )
      reachable
  let edges =
        [ ((renaming Map.! from, renaming Map.! to), lbl)
        | ((from, to), lbl) <- bgEdges build
        , from `Map.member` renaming
        , to `Map.member` renaming
        ]
      arrBounds = (0, n - 1)
  pure
    LocalGraph
      { lgGraph = G.buildG arrBounds (fmap fst edges)
      , lgStart = renaming Map.! oldStart
      , lgNodes = array arrBounds nodes
      , lgEdgeLabels = collectEdges edges
      , lgStartVarHints = ptHints target
      }
  where
    oldStart = ptVertex target

requireNode :: G.Vertex -> Map.Map G.Vertex LocalNode -> Either ProjectionError LocalNode
requireNode v nodes =
  maybe
    (Left (ProjectionError ("Projection internal error: missing local node metadata for vertex " ++ show v ++ ".")))
    Right
    (Map.lookup v nodes)

localOutgoing :: [((G.Vertex, G.Vertex), LocalEdgeLabel)] -> Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)]
localOutgoing =
  foldl' (\acc ((from, to), lbl) -> Map.insertWith (++) from [(lbl, to)] acc) Map.empty

bfsReachable :: Map.Map G.Vertex [(LocalEdgeLabel, G.Vertex)] -> G.Vertex -> [G.Vertex]
bfsReachable out start = go Set.empty [start] []
  where
    go _ [] acc = reverse acc
    go seen (v : vs) acc
      | v `Set.member` seen = go seen vs acc
      | otherwise =
          let succs = fmap snd (Map.findWithDefault [] v out)
           in go (Set.insert v seen) (vs ++ succs) (v : acc)

collectEdges :: Ord k => [(k, v)] -> Map.Map k [v]
collectEdges = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty

liftEither :: Either ProjectionError a -> ProjM a
liftEither (Right x) = pure x
liftEither (Left err) = StateT (\_ -> Left err)

failProjection :: String -> ProjM a
failProjection msg = liftEither (Left (ProjectionError msg))
