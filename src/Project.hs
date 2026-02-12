-- | Projection algorithms from global graphs to participant-local graphs.
--
-- The concrete algorithms are planned but currently not implemented.
module Project
  ( projectCoinductiveFull
  , projectCoinductivePlain
  , projectInductiveFull
  , projectInductivePlain
  , ProjectionError(..)
  , ProjectionResult
  ) where

import Automata (GlobalGraph, LocalGraph)
import Syntax.AST (Participant)

-- | Projection-specific failure.
data ProjectionError = ProjectionError String
  deriving (Eq, Show)

-- | Result type returned by projection algorithms.
type ProjectionResult = Either ProjectionError LocalGraph

-- | Coinductive projection including all checks/annotations ("full" variant).
projectCoinductiveFull :: GlobalGraph -> Participant -> ProjectionResult
projectCoinductiveFull _ _ = Left (ProjectionError "projectCoinductiveFull not implemented yet")

-- | Coinductive projection in the simplified/plain variant.
projectCoinductivePlain :: GlobalGraph -> Participant -> ProjectionResult
projectCoinductivePlain _ _ = Left (ProjectionError "projectCoinductivePlain not implemented yet")

-- | Inductive projection including all checks/annotations ("full" variant).
projectInductiveFull :: GlobalGraph -> Participant -> ProjectionResult
projectInductiveFull _ _ = Left (ProjectionError "projectInductiveFull not implemented yet")

-- | Inductive projection in the simplified/plain variant.
projectInductivePlain :: GlobalGraph -> Participant -> ProjectionResult
projectInductivePlain _ _ = Left (ProjectionError "projectInductivePlain not implemented yet")
