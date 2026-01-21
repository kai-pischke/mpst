module Syntax.AST
  ( Participant(..)
  , Label(..)
  , TypeVar(..)
  , Branches
  , GlobalType(..)
  , LocalType(..)
  ) where

import Data.List.NonEmpty (NonEmpty)

-- | Participant identifiers in a protocol.
newtype Participant = Participant { getParticipant :: String }
  deriving (Eq, Ord, Show)

-- | Message labels used to distinguish branches.
newtype Label = Label { getLabel :: String }
  deriving (Eq, Ord, Show)

-- | Type variables used for recursive types.
newtype TypeVar = TypeVar { getTypeVar :: String }
  deriving (Eq, Ord, Show)

-- | A non-empty set of labelled continuations.
type Branches t = NonEmpty (Label, t)

-- | Global types describe whole-protocol behaviour.
data GlobalType
  = GMessage Participant Participant (Branches GlobalType) -- ^ p -> q {l1: G1, ..., ln: Gn}
  | GVar TypeVar                                           -- ^ Type variable t
  | GRec TypeVar GlobalType                                -- ^ rec t . G
  | GEnd                                                   -- ^ end
  deriving (Eq, Show)

-- | Local types describe a single participant's behaviour.
data LocalType
  = LSend Participant (Branches LocalType) -- ^ Internal choice: p ! {l1: T1, ..., ln: Tn}
  | LRecv Participant (Branches LocalType) -- ^ External choice: p ? {l1: T1, ..., ln: Tn}
  | LVar TypeVar                           -- ^ Type variable t
  | LRec TypeVar LocalType                 -- ^ rec t . T
  | LEnd                                   -- ^ end
  deriving (Eq, Show)
