module AutomataReconstructionSpec (spec) where

import Automata
  ( LocalEdgeLabel(..)
  , LocalGraph(..)
  , GlobalEdgeLabel(..)
  , GlobalGraph(..)
  , RecVarHints(..)
  , buildGlobalGraph
  , buildLocalGraph
  , globalGraphToType
  , localGraphToType
  )
import Syntax
  ( TypeVar(..)
  , renderGlobalType
  , renderLocalType
  )
import qualified TestGenerators
import Test.Hspec
  ( Spec
  , describe
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, counterexample, forAll, resize, (===))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map

spec :: Spec
spec =
  describe "Automata reconstruction scaffolding" $ do
    prop "[AUTO2TYPE-G-PROP] global type -> graph -> type roundtrip preserves graph+hints semantics" $
      propGlobalGraphRoundtrip
    prop "[AUTO2TYPE-L-PROP] local type -> graph -> type roundtrip preserves graph+hints semantics" $
      propLocalGraphRoundtrip

propGlobalGraphRoundtrip :: Property
propGlobalGraphRoundtrip =
  forAll (resize 14 TestGenerators.genWellFormedGlobal) $ \g ->
    case globalGraphToType (buildGlobalGraph g) of
      Left err ->
        counterexample
          ("globalGraphToType failed with " ++ show err)
          False
      Right reconstructed ->
        counterexample
          ( "Expected (rendered): " ++ renderGlobalType g
              ++ "\nReconstructed (rendered): "
              ++ renderGlobalType reconstructed
          )
          ( canonicalizeGlobalHintNames (buildGlobalGraph reconstructed)
              === canonicalizeGlobalHintNames (buildGlobalGraph g)
          )

propLocalGraphRoundtrip :: Property
propLocalGraphRoundtrip =
  forAll (resize 14 TestGenerators.genWellFormedLocal) $ \l ->
    case localGraphToType (buildLocalGraph l) of
      Left err ->
        counterexample
          ("localGraphToType failed with " ++ show err)
          False
      Right reconstructed ->
        counterexample
          ( "Expected (rendered): " ++ renderLocalType l
              ++ "\nReconstructed (rendered): "
              ++ renderLocalType reconstructed
          )
          ( canonicalizeLocalHintNames (buildLocalGraph reconstructed)
              === canonicalizeLocalHintNames (buildLocalGraph l)
          )

canonicalizeGlobalHintNames :: GlobalGraph -> GlobalGraph
canonicalizeGlobalHintNames gg =
  gg
    { ggStartVarHints = canonStartHints
    , ggEdgeLabels = canonEdges
    }
  where
    transitions =
      sortOn
        (\(from, to, ix, lbl) -> (from, to, geLabel lbl, ix))
        [ (from, to, ix, lbl)
        | ((from, to), labels) <- Map.toList (ggEdgeLabels gg)
        , (ix, lbl) <- zip [(0 :: Int) ..] labels
        ]

    (nameMap0, nextIx0, canonStartHints) = renameRecVarHints Map.empty 1 (ggStartVarHints gg)
    (_, _, canonTransitionHints) =
      foldl'
        stepTransition
        (nameMap0, nextIx0, Map.empty)
        transitions

    stepTransition (nameMap, nextIx, acc) (from, to, ix, lbl) =
      let (nameMap', nextIx', hints') = renameRecVarHints nameMap nextIx (geTargetHints lbl)
       in (nameMap', nextIx', Map.insert (from, to, ix) hints' acc)

    canonEdges =
      Map.mapWithKey
        (\(from, to) labels -> fmap (rewriteLabel from to) (zip [(0 :: Int) ..] labels))
        (ggEdgeLabels gg)

    rewriteLabel from to (ix, lbl) =
      lbl {geTargetHints = Map.findWithDefault (geTargetHints lbl) (from, to, ix) canonTransitionHints}

canonicalizeLocalHintNames :: LocalGraph -> LocalGraph
canonicalizeLocalHintNames lg =
  lg
    { lgStartVarHints = canonStartHints
    , lgEdgeLabels = canonEdges
    }
  where
    transitions =
      sortOn
        (\(from, to, ix, lbl) -> (from, to, leDirection lbl, lePeer lbl, leLabel lbl, ix))
        [ (from, to, ix, lbl)
        | ((from, to), labels) <- Map.toList (lgEdgeLabels lg)
        , (ix, lbl) <- zip [(0 :: Int) ..] labels
        ]

    (nameMap0, nextIx0, canonStartHints) = renameRecVarHints Map.empty 1 (lgStartVarHints lg)
    (_, _, canonTransitionHints) =
      foldl'
        stepTransition
        (nameMap0, nextIx0, Map.empty)
        transitions

    stepTransition (nameMap, nextIx, acc) (from, to, ix, lbl) =
      let (nameMap', nextIx', hints') = renameRecVarHints nameMap nextIx (leTargetHints lbl)
       in (nameMap', nextIx', Map.insert (from, to, ix) hints' acc)

    canonEdges =
      Map.mapWithKey
        (\(from, to) labels -> fmap (rewriteLabel from to) (zip [(0 :: Int) ..] labels))
        (lgEdgeLabels lg)

    rewriteLabel from to (ix, lbl) =
      lbl {leTargetHints = Map.findWithDefault (leTargetHints lbl) (from, to, ix) canonTransitionHints}

renameHintList ::
  Map.Map String TypeVar ->
  Int ->
  [TypeVar] ->
  (Map.Map String TypeVar, Int, [TypeVar])
renameHintList nameMap nextIx hints =
  let (nameMap', nextIx', revHints) = foldl' step (nameMap, nextIx, []) hints
   in (nameMap', nextIx', reverse revHints)
  where
    step (names, ix, acc) (TypeVar raw) =
      case Map.lookup raw names of
        Just canonical ->
          (names, ix, canonical : acc)
        Nothing ->
          let canonical = TypeVar ("a" ++ show ix)
           in (Map.insert raw canonical names, ix + 1, canonical : acc)

renameMaybeHint ::
  Map.Map String TypeVar ->
  Int ->
  Maybe TypeVar ->
  (Map.Map String TypeVar, Int, Maybe TypeVar)
renameMaybeHint nameMap nextIx maybeHint =
  case maybeHint of
    Nothing -> (nameMap, nextIx, Nothing)
    Just hint ->
      let (nameMap', nextIx', hints') = renameHintList nameMap nextIx [hint]
       in case hints' of
            [renamed] -> (nameMap', nextIx', Just renamed)
            _ -> (nameMap', nextIx', Nothing)

renameRecVarHints ::
  Map.Map String TypeVar ->
  Int ->
  RecVarHints ->
  (Map.Map String TypeVar, Int, RecVarHints)
renameRecVarHints nameMap nextIx hints =
  let (nameMap', nextIx', binders') = renameHintList nameMap nextIx (rvhBinders hints)
      (nameMap'', nextIx'', preferred') =
        renameMaybeHint nameMap' nextIx' (rvhPreferredVar hints)
   in ( nameMap''
      , nextIx''
      , RecVarHints
          { rvhBinders = binders'
          , rvhPreferredVar = preferred'
          }
      )

