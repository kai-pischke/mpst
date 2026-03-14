module TypecheckSpec (spec) where

import Data.Either (isLeft, isRight)
import Syntax
  ( BinOp(..)
  , Expr(..)
  , LocalType
  , Process
  , parseLocalTypeChecked
  , parseProcessChecked
  )
import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)
import Typecheck (ExprType(..), inferExprType, typecheck)

spec :: Spec
spec = describe "6) Typechecking" $ do
  describe "T-Inact (PEnd vs LEnd)" $ do
    it "[TC-001] 0 implements end" $
      expectTypecheck "0" "end"

    it "[TC-002] 0 does not implement p ! {l: end}" $
      expectTypecheckFail "0" "p ! {l: end}"

  describe "T-Sel (PSend vs LSend)" $ do
    it "[TC-010] p ! l . 0 implements p ! {l: end}" $
      expectTypecheck "p ! l . 0" "p ! {l: end}"

    it "[TC-011] p ! l . 0 implements p ! {l: end, m: end} (selection from superset)" $
      expectTypecheck "p ! l . 0" "p ! {l: end, m: end}"

    it "[TC-012] p ! l . 0 fails against q ! {l: end} (wrong participant)" $
      expectTypecheckFail "p ! l . 0" "q ! {l: end}"

    it "[TC-013] p ! m . 0 fails against p ! {l: end} (label not in type)" $
      expectTypecheckFail "p ! m . 0" "p ! {l: end}"

    it "[TC-014] p ! l . 0 fails against p ? {l: end} (send vs recv)" $
      expectTypecheckFail "p ! l . 0" "p ? {l: end}"

  describe "T-Bra (PRecv vs LRecv)" $ do
    it "[TC-020] p ? {l: 0} implements p ? {l: end}" $
      expectTypecheck "p ? {l: 0}" "p ? {l: end}"

    it "[TC-021] p ? {l: 0, m: 0} implements p ? {l: end} (extra branches OK)" $
      expectTypecheck "p ? {l: 0, m: 0}" "p ? {l: end}"

    it "[TC-022] p ? {l: 0} fails against p ? {l: end, m: end} (missing branch)" $
      expectTypecheckFail "p ? {l: 0}" "p ? {l: end, m: end}"

  describe "T-Rec / T-Var (recursion)" $ do
    it "[TC-030] rec X . p ! l . X implements rec t . p ! {l: t}" $
      expectTypecheck "rec X . p ! l . X" "rec t . p ! {l: t}"

    it "[TC-031] rec X . p ? {go: X, stop: 0} implements rec t . p ? {go: t, stop: end}" $
      expectTypecheck
        "rec X . p ? {go: X, stop: 0}"
        "rec t . p ? {go: t, stop: end}"

  describe "T-Cond (PIf)" $ do
    it "[TC-050] if true then 0 else 0 implements end" $
      expectTypecheck "if true then 0 else 0" "end"

    it "[TC-051] if x > 0 then p ! ok . 0 else p ! ok . 0 implements p ! {ok: end}" $
      expectTypecheck
        "if x > 0 then p ! ok . 0 else p ! ok . 0"
        "p ! {ok: end}"

    it "[TC-052] if true then p ! l . 0 else 0 fails (else doesn't match)" $
      expectTypecheckFail
        "if true then p ! l . 0 else 0"
        "p ! {l: end}"

    it "[TC-053] if 42 then 0 else 0 fails (non-bool condition)" $
      expectTypecheckFail "if 42 then 0 else 0" "end"

  describe "Complex protocols" $ do
    it "[TC-060] ping-pong protocol" $
      expectTypecheck
        "rec X . p ? {ping: p ! pong . X, quit: 0}"
        "rec t . p ? {ping: p ! {pong: t}, quit: end}"

    it "[TC-063] rec with conditional branching" $
      expectTypecheck
        "rec X . if b then p ! l . X else p ! m . 0"
        "rec t . p ! {l: t, m: end}"

  describe "Complex recursive protocols" $ do
    it "[TC-070] period-2 recursion" $
      expectTypecheck
        "rec X . p ! l . p ! m . X"
        "rec t . p ! {l: p ! {m: t}}"

    it "[TC-071] pseudo-mutual recursion, same type" $
      expectTypecheck
        "rec X . p ! l . rec Y . p ! m . X"
        "rec t . p ! {l: p ! {m: t}}"

    it "[TC-072] period-2 process against period-1 type (subtype at rec boundary)" $
      expectTypecheck
        "rec X . p ! l . p ! l . X"
        "rec t . p ! {l: t}"

    it "[TC-073] period-1 process against period-2 type (converse unfolding)" $
      expectTypecheck
        "rec X . p ! l . X"
        "rec t . p ! {l: p ! {l: t}}"

    it "[TC-074] period-2 with selection from superset at second step" $
      expectTypecheck
        "rec X . p ! l . p ! m . X"
        "rec t . p ! {l: p ! {m: t, l: t}}"

    it "[TC-075] multi-participant recursive protocol" $
      expectTypecheck
        "rec X . p ? {req: q ! resp . X, done: 0}"
        "rec t . p ? {req: q ! {resp: t}, done: end}"

    it "[TC-076] conditional inside recursion (both branches satisfy type)" $
      expectTypecheck
        "rec X . if b then p ! l . X else p ! m . 0"
        "rec t . p ! {l: t, m: end}"

    it "[TC-077] period-2 process vs period-1 type with different labels (fail)" $
      expectTypecheckFail
        "rec X . p ! l . p ! m . X"
        "rec t . p ! {l: t}"

  describe "Expression type inference" $ do
    it "[TC-080] EInt infers to TInt" $
      inferExprType (EInt 42) `shouldSatisfy` (== Right TInt)

    it "[TC-081] EBool infers to TBool" $
      inferExprType (EBool True) `shouldSatisfy` (== Right TBool)

    it "[TC-082] EVar infers to TAny" $
      inferExprType (EVar "x") `shouldSatisfy` (== Right TAny)

    it "[TC-083] arithmetic produces TInt" $
      inferExprType (EBinOp Add (EInt 1) (EInt 2)) `shouldSatisfy` (== Right TInt)

    it "[TC-084] comparison produces TBool" $
      inferExprType (EBinOp Gt (EInt 1) (EInt 2)) `shouldSatisfy` (== Right TBool)

-- | Parse both process and local type, then typecheck. Expect success.
expectTypecheck :: String -> String -> IO ()
expectTypecheck procSrc typeSrc =
  case parseBoth procSrc typeSrc of
    Left err -> expectationFailure err
    Right (proc, ltype) ->
      typecheck proc ltype `shouldSatisfy` isRight

-- | Parse both process and local type, then typecheck. Expect failure.
expectTypecheckFail :: String -> String -> IO ()
expectTypecheckFail procSrc typeSrc =
  case parseBoth procSrc typeSrc of
    Left err -> expectationFailure ("Parse failed unexpectedly: " ++ err)
    Right (proc, ltype) ->
      typecheck proc ltype `shouldSatisfy` isLeft

parseBoth :: String -> String -> Either String (Process, LocalType)
parseBoth procSrc typeSrc = do
  proc <- parseProcessChecked procSrc
  ltype <- parseLocalTypeChecked typeSrc
  pure (proc, ltype)
