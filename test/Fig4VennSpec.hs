module Fig4VennSpec (spec) where

import Automata (ContextGraph, buildContextGraph, buildLocalGraph)
import DeadlockFreedom (checkDeadlockFreedom)
import Liveness (checkLiveness)
import Safety (checkSafety)
import Syntax (Participant(..), parseLocalTypeChecked)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)

data Classification = Classification
  { cLive :: Bool
  , cDeadlockFree :: Bool
  , cSafe :: Bool
  }
  deriving (Eq, Show)

-- Original paper has an issue where Delta8/Delta9 are the wrong way around
spec :: Spec
spec =
  describe "POPL 2025 Fig. 4 Venn-diagram contexts (Delta5-Delta9)" $ do
    it "[FIG4-001] Delta5 is live, deadlock free, and safe" $
      expectClassification (Classification True True True) delta5
    it "[FIG4-002] Delta6 is live and deadlock free, but not safe" $
      expectClassification (Classification True True False) delta6
    it "[FIG4-003] Delta7 is deadlock free, but not live and not safe" $
      expectClassification (Classification False True False) delta7
    it "[FIG4-004] Delta8 is safe, but not live and not deadlock free" $
      expectClassification (Classification False False True) delta8
    it "[FIG4-005] Delta9 is deadlock free and safe, but not live" $
      expectClassification (Classification False True True) delta9

expectClassification :: Classification -> [(String, String)] -> Expectation
expectClassification expected participantLocals =
  case parseAsContextGraph participantLocals of
    Left err -> expectationFailure err
    Right cg -> classifyContext cg `shouldBe` expected

classifyContext :: ContextGraph -> Classification
classifyContext cg =
  Classification
    { cLive = checkLiveness cg == Right ()
    , cDeadlockFree = checkDeadlockFreedom cg == Right ()
    , cSafe = checkSafety cg == Right ()
    }

delta5 :: [(String, String)]
delta5 =
  [ ( "q"
    , "p ? { l1: r ? { l2: end, l3: end }, l4: r ? { l2: end, l5: end } }"
    )
  , ("p", "q ! { l1: end, l4: end }")
  , ("r", "q ! { l2: end }")
  ]

delta6 :: [(String, String)]
delta6 =
  [ ("q", "p ? { l1: end, l2: end }")
  , ("p", "q ! { l1: end, l3: end }")
  ]

-- Modified from paper since we don't have payloads 
delta7 :: [(String, String)]
delta7 =
  [ ("q", "rec t . p ? { val_S: t }")
  , ("p", "rec t . q ! { val_S: t }")
  , ("r", "s ? { l2: end }")
  , ("s", "r ! { l1: end }")
  , ("u", "v ! { l1: end }")
  ]


delta8 :: [(String, String)]
delta8 =
  [ ("q", "p ? { val_S: end }")
  ]

delta9 :: [(String, String)]
delta9 =
  [ ("q", "rec t . p ? { val_S: t }")
  , ("p", "rec t . q ! { val_S: t }")
  , ("r", "s ? { val_bool: end }")
  ]

parseAsContextGraph :: [(String, String)] -> Either String ContextGraph
parseAsContextGraph participantLocals = do
  locals <- mapM parseOne participantLocals
  pure (buildContextGraph locals)
  where
    parseOne (participantName, localSrc) =
      case parseLocalTypeChecked localSrc of
        Left err ->
          Left
            ( "Local type for participant "
                ++ show participantName
                ++ " failed to parse/check:\n"
                ++ err
            )
        Right lt ->
          Right (Participant participantName, buildLocalGraph lt)
