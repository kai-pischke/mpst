module Main (main) where

import qualified BalancedSpec
import qualified AutomataReconstructionSpec
import qualified ContextRandomSpec
import qualified DeadlockFreedomSpec
import qualified Fig4VennSpec
import qualified LivenessSpec
import qualified ProjectionSpec
import qualified RoundtripSpec
import qualified SafetySpec
import qualified SubtypingSpec
import Test.Hspec (hspec)
import qualified WellFormedSpec

main :: IO ()
main =
  hspec $ do
    RoundtripSpec.spec
    AutomataReconstructionSpec.spec
    WellFormedSpec.spec
    BalancedSpec.spec
    DeadlockFreedomSpec.spec
    ContextRandomSpec.spec
    Fig4VennSpec.spec
    ProjectionSpec.spec
    SafetySpec.spec
    LivenessSpec.spec
    SubtypingSpec.spec
