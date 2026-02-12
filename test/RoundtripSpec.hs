module RoundtripSpec
  ( spec
  ) where

import Syntax
import TestGenerators (genWellFormedGlobal, genWellFormedLocal)
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
