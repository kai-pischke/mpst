module RoundtripSpec
  ( spec
  ) where

import Control.Monad (replicateM)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as Env
import Syntax
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec =
  describe "Roundtrip" $ do
    prop "global syntax roundtrip is stable" propGlobalRoundtrip
    prop "local syntax roundtrip is stable" propLocalRoundtrip

propGlobalRoundtrip :: Property
propGlobalRoundtrip =
  forAll genWellFormedGlobal $ \g ->
    case parseGlobalTypeChecked (renderGlobalType g) of
      Left err -> counterexample ("Parse failed: " ++ err ++ "\nRendered: " ++ renderGlobalType g) False
      Right g' -> g' === g

propLocalRoundtrip :: Property
propLocalRoundtrip =
  forAll genWellFormedLocal $ \t ->
    case parseLocalTypeChecked (renderLocalType t) of
      Left err -> counterexample ("Parse failed: " ++ err ++ "\nRendered: " ++ renderLocalType t) False
      Right t' -> t' === t

-- Generators

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
      dir <- elements [True, False] -- True send, False recv
      peer <- genParticipant
      branchCount <- chooseInt (1, 3)
      let branchLabels = take branchCount uniqueLabels
      subSizes <- splitSizes size branchCount
      branches <- mapM (genBranch dir peer) (zip branchLabels subSizes)
      pure $ if dir then LSend peer (NE.fromList branches) else LRecv peer (NE.fromList branches)
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
