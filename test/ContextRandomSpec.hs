module ContextRandomSpec (spec) where

import Automata
  ( ContextEdgeLabel(..)
  , ContextGraph(..)
  )
import qualified Data.Map.Strict as Map
import DeadlockFreedom (checkDeadlockFreedom)
import Safety (checkSafety)
import Syntax
import TestGenerators
  ( GeneratedContext(..)
  , genContext
  , labelPool
  , labelsOfLocalType
  , participantPool
  )
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

data ContextClass
  = SafeAndDeadlockFree
  | DeadlockFreeNotSafe
  | SafeNotDeadlockFree
  deriving (Eq, Show)

data FoundExample = FoundExample Int GeneratedContext

spec :: Spec
spec =
  describe "Random context generation" $ do
    it "[CTXGEN-001] generates tiny contexts with 1-3 participants and restricted labels" $
      checkGeneratorShape
    it "[CTXGEN-002] finds a safe and deadlock-free context" $
      expectFound SafeAndDeadlockFree
    it "[CTXGEN-003] finds a deadlock-free but not safe context" $
      expectFound DeadlockFreeNotSafe
    it "[CTXGEN-004] finds a safe but not deadlock-free context" $
      expectFound SafeNotDeadlockFree

checkGeneratorShape :: Expectation
checkGeneratorShape =
  if all isWellShaped samples
    then pure ()
    else expectationFailure "Generator produced a context outside the intended tiny/restricted space."
  where
    samples = sampleContexts 200

    isWellShaped generated =
      let ps = gcParticipants generated
          usedLabels = concatMap (labelsOfLocalType . snd) (gcLocals generated)
       in length ps >= 1
            && length ps <= 3
            && all (`elem` participantPool) ps
            && all (`elem` labelPool) usedLabels
            && all (\(_, t) -> validateLocalType t == Right ()) (gcLocals generated)

expectFound :: ContextClass -> Expectation
expectFound klass =
  case findExample 8000 klass of
    Just _found -> pure ()
    Nothing ->
      expectationFailure
        ( "Generator failed to find context class "
            ++ show klass
            ++ " within search budget."
        )

findExample :: Int -> ContextClass -> Maybe FoundExample
findExample limit klass = go 1
  where
    go seed
      | seed > limit = Nothing
      | matchesClass klass generated && isNonTrivial generated =
          Just (FoundExample seed generated)
      | otherwise = go (seed + 1)
      where
        generated = unGen genContext (mkQCGen seed) 6

sampleContexts :: Int -> [GeneratedContext]
sampleContexts n =
  [ unGen genContext (mkQCGen seed) 6
  | seed <- [1 .. n]
  ]

matchesClass :: ContextClass -> GeneratedContext -> Bool
matchesClass klass generated =
  case klass of
    SafeAndDeadlockFree ->
      isSafe && isDeadlockFree
    DeadlockFreeNotSafe ->
      isDeadlockFree && not isSafe
    SafeNotDeadlockFree ->
      isSafe && not isDeadlockFree
  where
    graph = gcGraph generated
    isSafe = checkSafety graph == Right ()
    isDeadlockFree = checkDeadlockFreedom graph == Right ()

isNonTrivial :: GeneratedContext -> Bool
isNonTrivial generated =
  length (gcParticipants generated) >= 2
    && any isSync edgeLabels
  where
    edgeLabels = concat (Map.elems (cgEdgeLabels (gcGraph generated)))

    isSync ContextSyncEdge{} = True
    isSync _ = False
