-- | Safety checking for global graphs.
--
-- The checking algorithm is planned but currently not implemented.
module Safety
  ( SafetyError(..)
  , SafetyResult
  , checkSafety
  ) where

import Automata (GlobalGraph)

-- | Placeholder until the safety-checking algorithm is implemented.
data SafetyError = SafetyError String
  deriving (Eq, Show)

-- | Result of safety checking.
type SafetyResult = Either [SafetyError] ()

-- | Check whether a global graph is safety-compliant.
checkSafety :: GlobalGraph -> SafetyResult
checkSafety _ = Left [SafetyError "checkSafety not implemented yet"]
