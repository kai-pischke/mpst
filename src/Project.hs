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

data ProjectionError = ProjectionError String
  deriving (Eq, Show)

type ProjectionResult = Either ProjectionError LocalGraph

projectCoinductiveFull :: GlobalGraph -> Participant -> ProjectionResult
projectCoinductiveFull _ _ = Left (ProjectionError "projectCoinductiveFull not implemented yet")

projectCoinductivePlain :: GlobalGraph -> Participant -> ProjectionResult
projectCoinductivePlain _ _ = Left (ProjectionError "projectCoinductivePlain not implemented yet")

projectInductiveFull :: GlobalGraph -> Participant -> ProjectionResult
projectInductiveFull _ _ = Left (ProjectionError "projectInductiveFull not implemented yet")

projectInductivePlain :: GlobalGraph -> Participant -> ProjectionResult
projectInductivePlain _ _ = Left (ProjectionError "projectInductivePlain not implemented yet")
