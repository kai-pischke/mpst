module TestGenerators
  ( genWellFormedGlobal
  , genWellFormedLocal
  , GeneratedContext(..)
  , participantPool
  , labelPool
  , genContext
  , labelsOfLocalType
  ) where

import Automata
  ( ContextGraph
  , LocalDirection(..)
  , buildContextGraph
  , buildLocalGraph
  )
import Control.Monad (replicateM)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as Env
import Syntax
import Test.QuickCheck

-- | Small generated context used in random-context tests.
data GeneratedContext = GeneratedContext
  { gcParticipants :: [Participant]
  , gcLocals :: [(Participant, LocalType)]
  , gcGraph :: ContextGraph
  }

participantPool :: [Participant]
participantPool = [Participant "p", Participant "q", Participant "r"]

labelPool :: [Label]
labelPool = [Label "l1", Label "l2", Label "l3"]

-- Well-formed syntax generators

genWellFormedGlobal :: Gen GlobalType
genWellFormedGlobal = sized $ \n -> genGlobal Env.empty (max 1 n)

genWellFormedLocal :: Gen LocalType
genWellFormedLocal = sized $ \n -> genLocal Env.empty (max 1 n)

genGlobal :: Env.Map TypeVar Bool -> Int -> Gen GlobalType
genGlobal env size
  | size <= 0 = genBaseGlobal env
  | otherwise =
      frequency
        [ (4, genMessage)
        , (1, genRec)
        , (1, genBaseGlobal env)
        ]
  where
    genMessage = do
      sender <- genParticipant
      receiver <- suchThat genParticipant (/= sender)
      branchCount <- chooseInt (1, 3)
      let branchLabels = take branchCount uniqueLabels
      subSizes <- splitSizes size branchCount
      branches <- mapM genBranch (zip branchLabels subSizes)
      pure $ GMessage sender receiver (NE.fromList branches)
    genBranch (lbl, sz) = do
      let env' = Env.map (const True) env
      t <- genGlobal env' (sz - 1)
      pure (lbl, t)
    genRec = do
      let newVar = freshVar env
      body <- genGlobal (Env.insert newVar False env) (size - 1)
      pure (GRec newVar body)

genBaseGlobal :: Env.Map TypeVar Bool -> Gen GlobalType
genBaseGlobal env =
  frequency $
    [ (1, pure GEnd)
    ]
      ++ guardedVars
  where
    guardedVars =
      case [v | (v, True) <- Env.toList env] of
        [] -> []
        vs -> [(2, GVar <$> elements vs)]

genLocal :: Env.Map TypeVar Bool -> Int -> Gen LocalType
genLocal env size
  | size <= 0 = genBaseLocal env
  | otherwise =
      frequency
        [ (4, genSendRecv)
        , (1, genRec)
        , (1, genBaseLocal env)
        ]
  where
    genSendRecv = do
      isSend <- elements [True, False]
      peer <- genParticipant
      branchCount <- chooseInt (1, 3)
      let branchLabels = take branchCount uniqueLabels
      subSizes <- splitSizes size branchCount
      branches <- mapM (genBranch isSend peer) (zip branchLabels subSizes)
      pure $ if isSend then LSend peer (NE.fromList branches) else LRecv peer (NE.fromList branches)
    genBranch _ _ (lbl, sz) = do
      let env' = Env.map (const True) env
      t <- genLocal env' (sz - 1)
      pure (lbl, t)
    genRec = do
      let newVar = freshVar env
      body <- genLocal (Env.insert newVar False env) (size - 1)
      pure (LRec newVar body)

genBaseLocal :: Env.Map TypeVar Bool -> Gen LocalType
genBaseLocal env =
  frequency $
    [ (1, pure LEnd)
    ]
      ++ guardedVars
  where
    guardedVars =
      case [v | (v, True) <- Env.toList env] of
        [] -> []
        vs -> [(2, LVar <$> elements vs)]

splitSizes :: Int -> Int -> Gen [Int]
splitSizes totalSize parts = do
  xs <- replicateM parts (chooseInt (0, max 0 (totalSize - 1)))
  let sumSizes = sum xs
  pure [max 1 (x + (totalSize `div` parts) - (sumSizes `div` parts)) | x <- xs]

genParticipant :: Gen Participant
genParticipant = Participant . ('p' :) . show <$> chooseInt (1, 6)

uniqueLabels :: [Label]
uniqueLabels = [Label ("l" ++ show n) | n <- [(1 :: Int) ..]]

freshVar :: Env.Map TypeVar Bool -> TypeVar
freshVar env =
  head $ filter (`Env.notMember` env) candidates
  where
    candidates = [TypeVar ("t" ++ show n) | n <- [(1 :: Int) ..]]

-- Context graph generators

genContext :: Gen GeneratedContext
genContext = do
  participantCount <- chooseInt (1, 3)
  shuffledParticipants <- shuffle participantPool
  let participants = sort (take participantCount shuffledParticipants)
  locals <- mapM (\p -> (,) p <$> genTinyWellFormedLocal p participants) participants
  let graph = buildContextGraph [(p, buildLocalGraph t) | (p, t) <- locals]
  pure $
    GeneratedContext
      { gcParticipants = participants
      , gcLocals = locals
      , gcGraph = graph
      }

genTinyWellFormedLocal :: Participant -> [Participant] -> Gen LocalType
genTinyWellFormedLocal self participants =
  genContextLocal self participants Env.empty 2

genContextLocal :: Participant -> [Participant] -> Env.Map TypeVar Bool -> Int -> Gen LocalType
genContextLocal self participants env depth
  | depth <= 0 = genContextBase env
  | otherwise =
      frequency
        [ (4, genSendRecv)
        , (1, genRec)
        , (1, genContextBase env)
        ]
  where
    genSendRecv = do
      dir <- elements [Send, Receive]
      peer <- genPeer self participants
      branchCount <- chooseInt (1, 2)
      branchLabels <- take branchCount <$> shuffle labelPool
      branches <- mapM (genBranch dir peer) branchLabels
      pure $
        case dir of
          Send -> LSend peer (NE.fromList branches)
          Receive -> LRecv peer (NE.fromList branches)

    genBranch _ _ lbl = do
      let env' = Env.map (const True) env
      cont <- genContextLocal self participants env' (depth - 1)
      pure (lbl, cont)

    genRec = do
      let v = freshVar env
      body <- genContextLocal self participants (Env.insert v False env) (depth - 1)
      pure (LRec v body)

genContextBase :: Env.Map TypeVar Bool -> Gen LocalType
genContextBase env =
  frequency $
    [ (3, pure LEnd)
    ]
      ++ guardedVars
  where
    guardedVars =
      case [v | (v, True) <- Env.toList env] of
        [] -> []
        vs -> [(1, LVar <$> elements vs)]

genPeer :: Participant -> [Participant] -> Gen Participant
genPeer self participants =
  case filter (/= self) participants of
    [] -> pure self
    peers -> elements peers

labelsOfLocalType :: LocalType -> [Label]
labelsOfLocalType = go
  where
    go (LSend _ branches) = labelsInBranches branches
    go (LRecv _ branches) = labelsInBranches branches
    go (LRec _ body) = go body
    go (LVar _) = []
    go LEnd = []

    labelsInBranches branches =
      [ lbl
      | (lbl, _) <- NE.toList branches
      ]
        ++ concatMap (go . snd) (NE.toList branches)
