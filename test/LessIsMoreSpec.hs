module LessIsMoreSpec (spec) where

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

spec :: Spec
spec =
  -- Less Is More: Multiparty Session Types Revisited (Example 2, Recursive two-buyers)
  -- Alceste Scalas, Nobuko Yoshida (2019)
  -- http://mrg.doc.ic.ac.uk/publications/less-is-more-multiparty-session-types-revisited/DTRS18-6.pdf
  describe "Less Is More (2019) examples" $ do
    it "[LIM-2019-001] Recursive two-buyers is safe and deadlock-free, but not live" $
      expectClassification
        (Classification False True True)
        deltaTwoBuyers
    -- Disabled for normal test runs: this liveness case is expensive.
    -- it "[LIM-2019-002] mapreduce_3 is safe, deadlock-free, and live" $
    --   expectClassification
    --     (Classification True True True)
    --     deltaMapReduce3

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

deltaTwoBuyers :: [(String, String)]
deltaTwoBuyers =
  [ ( "a"
    , "s ! { query: s ? { price: rec t . b ! { split: b ? { yes: s ! { buy: end }, no: t }, cancel: s ! { no: end } } } }"
    )
  , ( "s"
    , "a ? { query: a ! { price: a ? { buy: end, no: end } } }"
    )
  , ( "b"
    , "rec t . a ? { split: a ! { yes: end, no: t }, cancel: end }"
    )
  ]

-- deltaMapReduce3 :: [(String, String)]
-- deltaMapReduce3 =
--   [ ( "m"
--     , "rec t . w1 ! { datum: w2 ! { datum: w3 ! { datum: r ? { continue: t, stop: w1 ! { stop: w2 ! { stop: w3 ! { stop: end } } } } } } }"
--     )
--   , ( "w1"
--     , "m ? { datum: rec t . r ! { result: m ? { datum: t, stop: end } } }"
--     )
--   , ( "w2"
--     , "m ? { datum: rec t . r ! { result: m ? { datum: t, stop: end } } }"
--     )
--   , ( "w3"
--     , "m ? { datum: rec t . r ! { result: m ? { datum: t, stop: end } } }"
--     )
--   , ( "r"
--     , "rec t . w1 ? { result: w2 ? { result: w3 ? { result: m ! { continue: t, stop: end } } } }"
--     )
--   ]

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
