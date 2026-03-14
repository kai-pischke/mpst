module Main (main) where

import qualified BalancedSpec
import qualified AutomataReconstructionSpec
import qualified ContextRandomSpec
import qualified DeadlockFreedomSpec
import qualified Fig4VennSpec
import qualified LessIsMoreSpec
import qualified LivenessSpec
import qualified MergeSpec
import qualified MpstkBackendSpec
import qualified ProjectionSpec
import qualified RoundtripSpec
import qualified SafetySpec
import qualified SubtypingSpec
import qualified SynthesiseSpec
import qualified TypecheckSpec
import qualified InferSpec
import Test.Hspec (hspec)
import qualified ProcessSpec
import qualified QBFSpec
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
    LessIsMoreSpec.spec
    MergeSpec.spec
    MpstkBackendSpec.spec
    ProjectionSpec.spec
    SafetySpec.spec
    LivenessSpec.spec
    SubtypingSpec.spec
    ProcessSpec.spec
    SynthesiseSpec.spec
    TypecheckSpec.spec
    InferSpec.spec
    QBFSpec.spec
