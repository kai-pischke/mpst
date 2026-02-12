module BalancedSpec (spec) where

import Automata (GlobalGraph, buildGlobalGraph)
import Balanced (BalancedError(..), checkBalanced)
import Syntax (parseGlobalTypeChecked)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, pendingWith)

spec :: Spec
spec =
  describe "1) Balanced checking" $ do
    it "[BAL-001] rejects non-balanced recursive branch participation" $
      expectUnbalanced "rec t . p -> q {l1: t, l2: q -> r {l3: end}}"
    it "[BAL-002] accepts balanced recursive branch participation" $
      expectBalanced "rec t . p -> q {l1: q -> r {l4: t}, l2: q -> r {l3: end}}"
    it "[BAL-003] handles guarded recursion fixed points" $
      pendingWith "TODO: implement test once checkBalanced is implemented"
    it "[BAL-004] ignores unreachable nodes from the start state" $
      pendingWith "TODO: implement test once checkBalanced is implemented"

expectBalanced :: String -> Expectation
expectBalanced source =
  case parseAsGlobalGraph source of
    Left err -> expectationFailure err
    Right gg ->
      case checkBalanced gg of
        Left [BalancedNotImplemented] ->
          pendingWith "TODO: enable once checkBalanced is implemented"
        Left errs ->
          expectationFailure ("Expected balanced protocol, but checker returned: " ++ show errs)
        Right () -> pure ()

expectUnbalanced :: String -> Expectation
expectUnbalanced source =
  case parseAsGlobalGraph source of
    Left err -> expectationFailure err
    Right gg ->
      case checkBalanced gg of
        Left [BalancedNotImplemented] ->
          pendingWith "TODO: enable once checkBalanced is implemented"
        Left _ -> pure ()
        Right () ->
          expectationFailure "Expected unbalanced protocol, but checker returned Right ()."

parseAsGlobalGraph :: String -> Either String GlobalGraph
parseAsGlobalGraph source =
  case parseGlobalTypeChecked source of
    Left err ->
      Left ("Input should parse and be well-formed, but failed with:\n" ++ err)
    Right g ->
      Right (buildGlobalGraph g)
