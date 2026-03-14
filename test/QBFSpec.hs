module QBFSpec (spec) where

import Benchmark.QBF
import Benchmark.Types (LocalExample(..))
import qualified Data.Map.Strict as Map
import MpstkBackend (MpstkResults(..), mpstkVerify)
import Syntax (LocalType, Participant(..), parseLocalTypeChecked)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)

spec :: Spec
spec =
  describe "QBF encoder" $ do
    describe "parsing" $ do
      it "[QBF-PARSE-001] qbfTrue1 parses" $
        parsesOk (mkQBF qbfTrue1)

      it "[QBF-PARSE-002] qbfFalse1 parses" $
        parsesOk (mkQBF qbfFalse1)

      it "[QBF-PARSE-003] qbfTrue2 parses" $
        parsesOk (mkQBF qbfTrue2)

      it "[QBF-PARSE-004] qbfFalse2 parses" $
        parsesOk (mkQBF qbfFalse2)

      it "[QBF-PARSE-005] qbfGame (3-var) parses" $
        parsesOk (mkQBF qbfGame)

    describe "liveness (mpstk)" $ do
      it "[QBF-LIVE-001] true 1-var formula => live+" $
        expectMpstkLive (mkQBF qbfTrue1)

      it "[QBF-LIVE-002] false 1-var formula => not live+" $
        expectMpstkNotLive (mkQBF qbfFalse1)

      it "[QBF-LIVE-003] true 2-var formula => live+" $
        expectMpstkLive (mkQBF qbfTrue2)

      it "[QBF-LIVE-004] false 2-var formula => not live+" $
        expectMpstkNotLive (mkQBF qbfFalse2)

      it "[QBF-LIVE-005] 3-var game formula => live+" $
        expectMpstkLive (mkQBF qbfGame)

parsesOk :: LocalExample -> IO ()
parsesOk ex =
  case parseCtx ex of
    Left err -> expectationFailure ("Parse error: " ++ err)
    Right _  -> pure ()

parseCtx :: LocalExample -> Either String (Map.Map Participant LocalType)
parseCtx ex = do
  pairs <- mapM parseOne (leParticipants ex)
  pure (Map.fromList pairs)
  where
    parseOne (name, src) =
      case parseLocalTypeChecked src of
        Left err -> Left (name ++ ": " ++ err)
        Right lt -> Right (Participant name, lt)

expectMpstkLive :: LocalExample -> IO ()
expectMpstkLive ex =
  case parseCtx ex of
    Left err -> expectationFailure ("Parse error: " ++ err)
    Right ctx -> do
      result <- mpstkVerify ctx
      case result of
        Left err -> expectationFailure ("mpstk error: " ++ err)
        Right res -> mpstkLivePlus res `shouldBe` True

expectMpstkNotLive :: LocalExample -> IO ()
expectMpstkNotLive ex =
  case parseCtx ex of
    Left err -> expectationFailure ("Parse error: " ++ err)
    Right ctx -> do
      result <- mpstkVerify ctx
      case result of
        Left err -> expectationFailure ("mpstk error: " ++ err)
        Right res -> mpstkLivePlus res `shouldBe` False
