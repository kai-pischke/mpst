-- | Balancedness checking for global graphs.
--
-- The checking algorithm is planned but currently not implemented.
module Balanced
  ( BalancedError(..)
  , BalancedResult
  , checkBalanced
  ) where

import Automata (GlobalGraph)

-- | Errors reported by balancedness checking.
data BalancedError
  = BalancedNotImplemented
  | BalancedError String
  deriving (Eq, Show)

-- | Result of balancedness checking.
type BalancedResult = Either [BalancedError] ()

-- | Check whether a global graph satisfies the balancedness criterion.
checkBalanced :: GlobalGraph -> BalancedResult
checkBalanced _ = Left [BalancedNotImplemented]
