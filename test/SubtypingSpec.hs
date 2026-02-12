module SubtypingSpec (spec) where

import Automata (LocalGraph, buildLocalGraph)
import Data.Either (isLeft)
import Subtyping (checkLocalSubtype)
import Syntax (parseLocalTypeChecked)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "5) Local subtyping" $ do
    it "[SUB-001] selection is covariant in branch sets" $
      expectSubtype
        "p ! {l1: end}"
        "p ! {l1: end, l2: end}"
    it "[SUB-002] branching is contravariant in branch sets" $
      expectSubtype
        "p ? {l1: end, l2: end}"
        "p ? {l1: end}"
    it "[SUB-003] allows recursion unfolding on the left" $
      expectSubtype
        "rec t . p ! {l1: p ! {l1: t}}"
        "rec t . p ! {l1: t}"
    it "[SUB-004] allows recursion unfolding on the right" $
      expectSubtype
        "rec t . p ! {l1: t}"
        "rec t . p ! {l1: p ! {l1: t}}"
    it "[SUB-005] rejects incompatible action kinds" $
      expectNotSubtype
        "p ! {l1: end}"
        "p ? {l1: end}"
    it "[SUB-006] rejects wrong participant" $
      expectNotSubtype
        "p ! {l1: end}"
        "q ! {l1: end}"

expectSubtype :: String -> String -> Expectation
expectSubtype leftSrc rightSrc =
  case parseSubtypePair leftSrc rightSrc of
    Left err -> expectationFailure err
    Right (leftGraph, rightGraph) ->
      checkLocalSubtype leftGraph rightGraph `shouldBe` Right ()

expectNotSubtype :: String -> String -> Expectation
expectNotSubtype leftSrc rightSrc =
  case parseSubtypePair leftSrc rightSrc of
    Left err -> expectationFailure err
    Right (leftGraph, rightGraph) ->
      checkLocalSubtype leftGraph rightGraph `shouldSatisfy` isLeft

parseSubtypePair :: String -> String -> Either String (LocalGraph, LocalGraph)
parseSubtypePair leftSrc rightSrc = do
  left <- parseLocalGraph "left (subtype candidate)" leftSrc
  right <- parseLocalGraph "right (supertype candidate)" rightSrc
  pure (left, right)

parseLocalGraph :: String -> String -> Either String LocalGraph
parseLocalGraph side source =
  case parseLocalTypeChecked source of
    Left err ->
      Left
        ( "Local type for "
            ++ side
            ++ " should parse and be well-formed, but failed with:\n"
            ++ err
        )
    Right localType ->
      Right (buildLocalGraph localType)
