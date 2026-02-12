module LivenessSpec (spec) where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec =
  describe "4) Liveness checking" $ do
    it "[LIVE-001] accepts a terminating protocol without deadlock" $
      pendingWith "TODO: implement when checkLiveness is implemented"
    it "[LIVE-002] rejects a deadlocked communication cycle" $
      pendingWith "TODO: implement when checkLiveness is implemented"
    it "[LIVE-003] rejects stuck recursion with no progress" $
      pendingWith "TODO: implement when checkLiveness is implemented"
    it "[LIVE-004] accepts productive recursion that can always advance" $
      pendingWith "TODO: implement when checkLiveness is implemented"
