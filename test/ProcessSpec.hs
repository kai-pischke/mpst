module ProcessSpec (spec) where

import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Syntax
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "Process" $ do
    describe "Parsing" $ do
      it "parses termination" $
        parseProcessChecked "0" `shouldBe` Right PEnd

      it "parses send" $
        parseProcessChecked "p ! ok . 0"
          `shouldBe` Right (PSend (Participant "p") (Label "ok") PEnd)

      it "parses receive with single branch" $
        parseProcessChecked "p ? {l1: 0}"
          `shouldBe` Right (PRecv (Participant "p") ((Label "l1", PEnd) :| []))

      it "parses receive with multiple branches" $
        parseProcessChecked "p ? {l1: 0, l2: 0}"
          `shouldBe` Right (PRecv (Participant "p")
            ((Label "l1", PEnd) :| [(Label "l2", PEnd)]))

      it "parses conditional" $
        parseProcessChecked "if true then 0 else 0"
          `shouldBe` Right (PIf (EBool True) PEnd PEnd)

      it "parses conditional with expression" $
        parseProcessChecked "if x > 0 then p ! ok . 0 else p ! fail . 0"
          `shouldBe` Right (PIf
            (EBinOp Gt (EVar "x") (EInt 0))
            (PSend (Participant "p") (Label "ok") PEnd)
            (PSend (Participant "p") (Label "fail") PEnd))

      it "parses recursion" $
        parseProcessChecked "rec X . server ? {req: client ! resp . X, quit: 0}"
          `shouldBe` Right (PRec (TypeVar "X")
            (PRecv (Participant "server")
              ( (Label "req", PSend (Participant "client") (Label "resp") (PVar (TypeVar "X")))
              :| [(Label "quit", PEnd)])))

      it "parses process variable" $
        parseProcess "X" `shouldBe` Right (PVar (TypeVar "X"))

      it "parses parenthesised process" $
        parseProcess "(0)" `shouldBe` Right PEnd

    describe "Expression parsing" $ do
      it "parses integer literal" $
        parseProcess "if 42 then 0 else 0"
          `shouldBe` Right (PIf (EInt 42) PEnd PEnd)

      it "parses boolean literals" $ do
        parseProcess "if true then 0 else 0"
          `shouldBe` Right (PIf (EBool True) PEnd PEnd)
        parseProcess "if false then 0 else 0"
          `shouldBe` Right (PIf (EBool False) PEnd PEnd)

      it "parses arithmetic" $
        parseProcess "if x + 1 > 0 then 0 else 0"
          `shouldBe` Right (PIf
            (EBinOp Gt (EBinOp Add (EVar "x") (EInt 1)) (EInt 0))
            PEnd PEnd)

      it "parses logical operators" $
        parseProcess "if a && b || c then 0 else 0"
          `shouldBe` Right (PIf
            (EBinOp Or (EBinOp And (EVar "a") (EVar "b")) (EVar "c"))
            PEnd PEnd)

      it "parses not" $
        parseProcess "if not x then 0 else 0"
          `shouldBe` Right (PIf (ENot (EVar "x")) PEnd PEnd)

      it "respects precedence: * binds tighter than +" $
        parseProcess "if a + b * c then 0 else 0"
          `shouldBe` Right (PIf
            (EBinOp Add (EVar "a") (EBinOp Mul (EVar "b") (EVar "c")))
            PEnd PEnd)

      it "parses equality and inequality" $
        parseProcess "if x == 1 then 0 else 0"
          `shouldBe` Right (PIf (EBinOp Eq (EVar "x") (EInt 1)) PEnd PEnd)

      it "parses parenthesised expressions" $
        parseProcess "if (a + b) * c then 0 else 0"
          `shouldBe` Right (PIf
            (EBinOp Mul (EBinOp Add (EVar "a") (EVar "b")) (EVar "c"))
            PEnd PEnd)

    describe "Pretty-printing" $ do
      it "renders termination" $
        renderProcess PEnd `shouldBe` "0"

      it "renders send" $
        renderProcess (PSend (Participant "p") (Label "ok") PEnd)
          `shouldBe` "p ! ok . 0"

      it "renders receive" $
        renderProcess (PRecv (Participant "p") ((Label "l1", PEnd) :| [(Label "l2", PEnd)]))
          `shouldBe` "p ? {l1: 0, l2: 0}"

      it "renders conditional" $
        renderProcess (PIf (EBool True) PEnd PEnd)
          `shouldBe` "if true then 0 else 0"

      it "renders recursion" $
        renderProcess (PRec (TypeVar "X") (PVar (TypeVar "X")))
          `shouldBe` "rec X . X"

      it "renders expressions with correct precedence" $
        renderProcess (PIf (EBinOp Add (EVar "a") (EBinOp Mul (EVar "b") (EVar "c"))) PEnd PEnd)
          `shouldBe` "if a + b * c then 0 else 0"

      it "renders not expression" $
        renderProcess (PIf (ENot (EVar "x")) PEnd PEnd)
          `shouldBe` "if not x then 0 else 0"

    describe "Roundtrip" $ do
      it "roundtrips simple send" $
        roundtrip "p ! ok . 0"

      it "roundtrips receive" $
        roundtrip "p ? {l1: 0, l2: 0}"

      it "roundtrips conditional with expression" $
        roundtrip "if x > 0 then p ! ok . 0 else p ! fail . 0"

      it "roundtrips recursion" $
        roundtrip "rec X . server ? {req: client ! resp . X, quit: 0}"

      it "roundtrips complex expression" $
        roundtrip "if a + b * c > 0 && not d then 0 else 0"

    describe "Well-formedness" $ do
      it "accepts valid process" $
        validateProcess (PSend (Participant "p") (Label "ok") PEnd)
          `shouldBe` Right ()

      it "accepts guarded recursion" $
        validateProcess (PRec (TypeVar "X")
          (PSend (Participant "p") (Label "ok") (PVar (TypeVar "X"))))
          `shouldBe` Right ()

      it "detects free process variable" $
        validateProcess (PVar (TypeVar "X"))
          `shouldBe` Left [FreeProcessVar (TypeVar "X")]

      it "detects unguarded process variable" $
        validateProcess (PRec (TypeVar "X") (PVar (TypeVar "X")))
          `shouldBe` Left [UnguardedProcessVar (TypeVar "X")]

      it "detects unguarded through if" $
        validateProcess (PRec (TypeVar "X") (PIf (EBool True) (PVar (TypeVar "X")) PEnd))
          `shouldBe` Left [UnguardedProcessVar (TypeVar "X")]

      it "accepts guarded through send in if branches" $
        validateProcess (PRec (TypeVar "X")
          (PSend (Participant "p") (Label "ok")
            (PIf (EBool True) (PVar (TypeVar "X")) PEnd)))
          `shouldBe` Right ()

      it "detects duplicate process labels" $
        validateProcess (PRecv (Participant "p")
          ((Label "dup", PEnd) :| [(Label "dup", PEnd)]))
          `shouldBe` Left [DuplicateProcessLabel (Label "dup")]

      it "parseProcessChecked reports unguarded recursion" $
        parseProcessChecked "rec X . X" `shouldSatisfy` hasError "UnguardedProcessVar"

      it "parseProcessChecked accepts valid process" $
        parseProcessChecked "rec X . p ! ok . X" `shouldSatisfy` isRight

roundtrip :: String -> IO ()
roundtrip input =
  case parseProcessChecked input of
    Left err -> error ("Parse failed: " ++ err)
    Right p  -> renderProcess p `shouldBe` input

hasError :: String -> Either String a -> Bool
hasError needle (Left err) = needle `isInfixOf` err
hasError _ (Right _)       = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
