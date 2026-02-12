module SafetySpec (spec) where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec =
  describe "3) Safety checking" $ do
    it "[SAFE-001] accepts a protocol with matched sends and receives" $
      pendingWith "TODO: implement when checkSafety is implemented"
    it "[SAFE-002] rejects orphan receive actions" $
      pendingWith "TODO: implement when checkSafety is implemented"
    it "[SAFE-003] rejects send/send race on the same channel state" $
      pendingWith "TODO: implement when checkSafety is implemented"
    it "[SAFE-004] checks safety under recursion unfolding" $
      pendingWith "TODO: implement when checkSafety is implemented"
