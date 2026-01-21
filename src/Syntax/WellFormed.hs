module Syntax.WellFormed
  ( WFError(..)
  , checkGlobalType
  , checkLocalType
  , validateGlobalType
  , validateLocalType
  , parseGlobalTypeChecked
  , parseLocalTypeChecked
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as Env
import qualified Data.Set as Set
import Syntax.AST
import Syntax.Parser (parseGlobalType, parseLocalType)
import Text.Megaparsec (errorBundlePretty)

data WFError
  = FreeTypeVar TypeVar
  | UnguardedTypeVar TypeVar
  | SelfCommunication Participant
  | DuplicateLabel Label
  deriving (Eq, Ord, Show)

validateGlobalType :: GlobalType -> Either [WFError] ()
validateGlobalType g =
  case checkGlobalType g of
    [] -> Right ()
    es -> Left es

validateLocalType :: LocalType -> Either [WFError] ()
validateLocalType t =
  case checkLocalType t of
    [] -> Right ()
    es -> Left es

parseGlobalTypeChecked :: String -> Either String GlobalType
parseGlobalTypeChecked src = do
  g <- firstParse (parseGlobalType src)
  case validateGlobalType g of
    Left es -> Left (renderErrors es)
    Right _ -> Right g

parseLocalTypeChecked :: String -> Either String LocalType
parseLocalTypeChecked src = do
  t <- firstParse (parseLocalType src)
  case validateLocalType t of
    Left es -> Left (renderErrors es)
    Right _ -> Right t

firstParse :: Show e => Either e b -> Either String b
firstParse = either (Left . show) Right

renderErrors :: [WFError] -> String
renderErrors = unlines . fmap show

checkGlobalType :: GlobalType -> [WFError]
checkGlobalType = checkGlobal Env.empty

checkGlobal :: Env.Map TypeVar Bool -> GlobalType -> [WFError]
checkGlobal env gtype =
  case gtype of
    GMessage sender receiver branches ->
      let dupErrs = duplicateLabelErrors (fmap fst (NE.toList branches))
          selfErr = [SelfCommunication sender | sender == receiver]
          env' = Env.map (const True) env
          branchErrs = concatMap (checkGlobal env' . snd) (NE.toList branches)
       in dupErrs ++ selfErr ++ branchErrs
    GVar v ->
      case Env.lookup v env of
        Nothing -> [FreeTypeVar v]
        Just guarded ->
          if guarded then [] else [UnguardedTypeVar v]
    GRec v body ->
      checkGlobal (Env.insert v False env) body
    GEnd -> []

checkLocalType :: LocalType -> [WFError]
checkLocalType = checkLocal Env.empty

checkLocal :: Env.Map TypeVar Bool -> LocalType -> [WFError]
checkLocal env ltype =
  case ltype of
    LSend _ branches ->
      branchChecks branches env
    LRecv _ branches ->
      branchChecks branches env
    LVar v ->
      case Env.lookup v env of
        Nothing -> [FreeTypeVar v]
        Just guarded ->
          if guarded then [] else [UnguardedTypeVar v]
    LRec v body ->
      checkLocal (Env.insert v False env) body
    LEnd -> []

branchChecks :: NE.NonEmpty (Label, LocalType) -> Env.Map TypeVar Bool -> [WFError]
branchChecks branches env =
  let dupErrs = duplicateLabelErrors (fmap fst (NE.toList branches))
      env' = Env.map (const True) env
      branchErrs = concatMap (\(_, t) -> checkLocal env' t) (NE.toList branches)
   in dupErrs ++ branchErrs

duplicateLabelErrors :: [Label] -> [WFError]
duplicateLabelErrors = go Set.empty
  where
    go _ [] = []
    go seen (l@(Label name) : xs)
      | name `Set.member` seen = DuplicateLabel l : go seen xs
      | otherwise = go (Set.insert name seen) xs
