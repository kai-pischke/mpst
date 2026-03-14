module AutomataReconstructionSpec (spec) where

import Automata
  ( buildGlobalGraph
  , buildLocalGraph
  , globalGraphToType
  , localGraphToType
  )
import Syntax
  ( renderGlobalType
  , renderLocalType
  )
import qualified TestGenerators
import Test.Hspec
  ( Spec
  , describe
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, counterexample, forAll, resize, (===))

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
          (reconstructed === g)

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
          (reconstructed === l)
