module ProjectionSpec (spec) where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec =
  describe "2) Projection algorithms" $ do
    it "[PROJ-IF-001] inductive-full projects a simple choice protocol" $
      pendingWith "TODO: implement when projectInductiveFull is implemented"
    it "[PROJ-IF-002] inductive-full rejects non-projectable branching" $
      pendingWith "TODO: implement when projectInductiveFull is implemented"
    it "[PROJ-IP-001] inductive-plain projects a simple recursive protocol" $
      pendingWith "TODO: implement when projectInductivePlain is implemented"
    it "[PROJ-IP-002] inductive-plain preserves branch labels at local endpoints" $
      pendingWith "TODO: implement when projectInductivePlain is implemented"
    it "[PROJ-CF-001] coinductive-full accepts productive recursion" $
      pendingWith "TODO: implement when projectCoinductiveFull is implemented"
    it "[PROJ-CF-002] coinductive-full reports merge incompatibility" $
      pendingWith "TODO: implement when projectCoinductiveFull is implemented"
    it "[PROJ-CP-001] coinductive-plain projects finite acyclic protocols" $
      pendingWith "TODO: implement when projectCoinductivePlain is implemented"
    it "[PROJ-CP-002] coinductive-plain rejects ambiguous participant views" $
      pendingWith "TODO: implement when projectCoinductivePlain is implemented"
