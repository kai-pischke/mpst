module Main (main) where

import Control.Monad (replicateM)
import Data.List (nub)
import qualified Data.Map.Lazy as Env
import qualified Data.List.NonEmpty as NE
import Syntax
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck prop_global_roundtrip
  quickCheck prop_local_roundtrip

prop_global_roundtrip :: Property
prop_global_roundtrip =
  forAll genWellFormedGlobal $ \g ->
    case parseGlobalTypeChecked (renderGlobalType g) of
      Left err -> counterexample ("Parse failed: " ++ err ++ "\nRendered: " ++ renderGlobalType g) False
      Right g' -> g' === g

prop_local_roundtrip :: Property
prop_local_roundtrip =
  forAll genWellFormedLocal $ \t ->
    case parseLocalTypeChecked (renderLocalType t) of
      Left err -> counterexample ("Parse failed: " ++ err ++ "\nRendered: " ++ renderLocalType t) False
      Right t' -> t' === t

-- Generators

genWellFormedGlobal :: Gen GlobalType
genWellFormedGlobal = sized $ \n -> genGlobal Env.empty False (max 1 n)

genWellFormedLocal :: Gen LocalType
genWellFormedLocal = sized $ \n -> genLocal Env.empty False (max 1 n)

genGlobal :: Env.Map TypeVar Bool -> Bool -> Int -> Gen GlobalType
genGlobal env guarded size
  | size <= 0 = genBaseGlobal env guarded
  | otherwise =
      frequency
        [ (4, genMessage)
        , (1, genRec)
        , (1, genBaseGlobal env guarded)
        ]
  where
    genMessage = do
      sender <- genParticipant
      receiver <- suchThat genParticipant (/= sender)
      branchCount <- chooseInt (1, 3)
      let labels = take branchCount uniqueLabels
      subSizes <- splitSizes size branchCount
      branches <- mapM genBranch (zip labels subSizes)
      pure $ GMessage sender receiver (NE.fromList branches)
    genBranch (lbl, sz) = do
      let env' = Env.map (const True) env
      t <- genGlobal env' True (sz - 1)
      pure (lbl, t)
    genRec = do
      let newVar = freshVar env
      body <- genGlobal (Env.insert newVar False env) guarded (size - 1)
      pure (GRec newVar body)

genBaseGlobal :: Env.Map TypeVar Bool -> Bool -> Gen GlobalType
genBaseGlobal env guarded =
  frequency $
    [ (1, pure GEnd)
    ]
      ++ guardedVars
  where
    guardedVars =
      case [v | (v, True) <- Env.toList env] of
        [] -> []
        vs -> [(2, GVar <$> elements vs)]

genLocal :: Env.Map TypeVar Bool -> Bool -> Int -> Gen LocalType
genLocal env guarded size
  | size <= 0 = genBaseLocal env guarded
  | otherwise =
      frequency
        [ (4, genSendRecv)
        , (1, genRec)
        , (1, genBaseLocal env guarded)
        ]
  where
    genSendRecv = do
      dir <- elements [True, False] -- True send, False recv
      peer <- genParticipant
      branchCount <- chooseInt (1, 3)
      let labels = take branchCount uniqueLabels
      subSizes <- splitSizes size branchCount
      branches <- mapM (genBranch dir peer) (zip labels subSizes)
      pure $ if dir then LSend peer (NE.fromList branches) else LRecv peer (NE.fromList branches)
    genBranch _ _ (lbl, sz) = do
      let env' = Env.map (const True) env
      t <- genLocal env' True (sz - 1)
      pure (lbl, t)
    genRec = do
      let newVar = freshVar env
      body <- genLocal (Env.insert newVar False env) guarded (size - 1)
      pure (LRec newVar body)

genBaseLocal :: Env.Map TypeVar Bool -> Bool -> Gen LocalType
genBaseLocal env guarded =
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
splitSizes total parts = do
  xs <- replicateM parts (chooseInt (0, max 0 (total - 1)))
  let s = sum xs
  pure [max 1 (x + (total `div` parts) - (s `div` parts)) | x <- xs]

genParticipant :: Gen Participant
genParticipant = Participant . ('p' :) . show <$> chooseInt (1, 6)

uniqueLabels :: [Label]
uniqueLabels = [Label ("l" ++ show n) | n <- [(1 :: Int) ..]]

freshVar :: Env.Map TypeVar Bool -> TypeVar
freshVar env =
  head $ filter (`Env.notMember` env) candidates
  where
    candidates = [TypeVar ("t" ++ show n) | n <- [(1 :: Int) ..]]
