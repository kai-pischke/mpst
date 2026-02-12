-- | Liveness checking for global graphs.
--
-- The checking algorithm is planned but currently not implemented.
module Liveness
  ( LivenessError(..)
  , LivenessResult
  , checkLiveness
  ) where

import Automata (GlobalGraph)

-- | Placeholder until the liveness-checking algorithm is implemented.
data LivenessError = LivenessError String
  deriving (Eq, Show)

-- | Result of liveness checking.
type LivenessResult = Either [LivenessError] ()

-- | Check whether a global graph satisfies liveness/progress requirements.
checkLiveness :: GlobalGraph -> LivenessResult
checkLiveness _ = Left [LivenessError "checkLiveness not implemented yet"]
