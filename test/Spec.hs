module Main (main) where

import qualified BalancedSpec
import qualified LivenessSpec
import qualified ProjectionSpec
import qualified RoundtripSpec
import qualified SafetySpec
import Test.Hspec (hspec)
import qualified WellFormedSpec

main :: IO ()
main =
  hspec $ do
    RoundtripSpec.spec
    WellFormedSpec.spec
    BalancedSpec.spec
    ProjectionSpec.spec
    SafetySpec.spec
    LivenessSpec.spec
