module ProjectionSpec (spec) where

import Automata (GlobalGraph, buildGlobalGraph, localGraphToType)
import Project
  ( ProjectionResult
  , projectCoinductiveFull
  , projectCoinductivePlain
  , projectInductiveFull
  , projectInductivePlain
  )
import Syntax
  ( LocalType
  , Participant(..)
  , alphaEqLocalType
  , normalizeLocalBranchOrder
  , parseGlobalTypeChecked
  , parseLocalTypeChecked
  )
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it)

spec :: Spec
spec =
  describe "2) Projection algorithms" $ do
    it "[PROJ-IF-001] inductive-full projects a simple choice protocol" $
      expectProjectionAs
        projectInductiveFull
        "r -> s {a: q -> p {l1: end}, b: q -> p {l1: end, l2: end}}"
        "p"
        "q ? {l1: end, l2: end}"

    it "[PROJ-IF-002] inductive-full rejects non-projectable branching" $
      expectProjectionFails
        projectInductiveFull
        gSendConflict
        "p"

    it "[PROJ-IF-003] inductive-full projects the same global onto q" $
      expectProjectionAs
        projectInductiveFull
        gSendConflict
        "q"
        "p ? {l1: end, l2: end}"

    it "[PROJ-IP-001] inductive-plain projects a simple recursive protocol" $
      expectProjectionAs
        projectInductivePlain
        "rec t . r -> s {x: p -> q {l: t}}"
        "p"
        "rec t . q ! {l: t}"

    it "[PROJ-IP-002] inductive-plain preserves branch labels at local endpoints" $
      expectProjectionAs
        projectInductivePlain
        "p -> q {k: r -> s {x: q -> p {ack: end}, y: q -> p {ack: end}}}"
        "p"
        "q ! {k: q ? {ack: end}}"

    -- Example 4.8 in Top-Down or Bottom-Up? (POPL 2025):
    -- https://arxiv.org/abs/2411.07452
    it "[PROJ-EX48-001] G_ip is projectable by inductive-plain" $
      expectProjectionAs
        projectInductivePlain
        gIp
        "p"
        "rec t . r ? {l1: t}"

    it "[PROJ-EX48-002] G_ip is projectable by inductive-full" $
      expectProjectionAs
        projectInductiveFull
        gIp
        "p"
        "rec t . r ? {l1: t}"

    it "[PROJ-EX48-003] G_if is not projectable by inductive-plain" $
      expectProjectionFails
        projectInductivePlain
        gIf
        "p"

    it "[PROJ-EX48-004] G_if is projectable by inductive-full" $
      expectProjectionAs
        projectInductiveFull
        gIf
        "p"
        "rec t . r ? {l1: t, l2: end}"

    -- Example from:
    -- Dawit Tirore, Jesper Bengtson, Marco Carbone.
    -- A Sound and Complete Projection for Global Types (ITP 2023).
    it "[PROJ-ITP23-001] G is not projectable by inductive-plain" $
      expectProjectionFails
        projectInductivePlain
        gItp23
        "Alice"

    it "[PROJ-ITP23-002] G is not projectable by inductive-full" $
      expectProjectionFails
        projectInductiveFull
        gItp23
        "Alice"

    it "[PROJ-ITP23-003] G is projectable by coinductive-plain" $
      expectProjectionSucceeds
        projectCoinductivePlain
        gItp23
        "Alice"

    it "[PROJ-ITP23-004] G is projectable by coinductive-full" $
      expectProjectionSucceeds
        projectCoinductiveFull
        gItp23
        "Alice"

    -- Ring protocol example from:
    -- David Castro-Perez, Francisco Ferreira, Sung-Shik Jongmans.
    -- A Synthetic Reconstruction of Multiparty Session Types (POPL 2026).
    it "[PROJ-RING-001] G_Ring is not projectable onto c by inductive-plain" $
      expectProjectionFails
        projectInductivePlain
        gRing
        "c"

    it "[PROJ-RING-002] G_Ring is projectable onto c by inductive-full" $
      expectProjectionAs
        projectInductiveFull
        gRing
        "c"
        "b ? { AppThenGet: a ! {Val: end}, App: a ? {Get: a ! {Val: end}} }"

    it "[PROJ-RING-003] G_Ring is not projectable onto c by coinductive-plain" $
      expectProjectionFails
        projectCoinductivePlain
        gRing
        "c"

    it "[PROJ-RING-004] G_Ring is projectable onto c by coinductive-full" $
      expectProjectionAs
        projectCoinductiveFull
        gRing
        "c"
        "b ? { AppThenGet: a ! {Val: end}, App: a ? {Get: a ! {Val: end}} }"

    -- OAuth2 fragment (Example (1)) from:
    -- Less Is More: Multiparty Session Types Revisited
    -- Alceste Scalas, Nobuko Yoshida.
    it "[PROJ-OAUTH-001] G_OAuth is not projectable onto a by inductive-plain" $
      expectProjectionFails
        projectInductivePlain
        gOAuth
        "a"

    it "[PROJ-OAUTH-002] G_OAuth is projectable onto a by inductive-full" $
      expectProjectionAs
        projectInductiveFull
        gOAuth
        "a"
        "c ? { passwd: s ! {auth: end}, quit: end }"

    it "[PROJ-OAUTH-003] G_OAuth is not projectable onto a by coinductive-plain" $
      expectProjectionFails
        projectCoinductivePlain
        gOAuth
        "a"

    it "[PROJ-OAUTH-004] G_OAuth is projectable onto a by coinductive-full" $
      expectProjectionAs
        projectCoinductiveFull
        gOAuth
        "a"
        "c ? { passwd: s ! {auth: end}, quit: end }"

    it "[PROJ-CF-001] coinductive-full projects all currently projectable fixtures" $
      mapM_
        (\(gSrc, participant) -> expectProjectionSucceeds projectCoinductiveFull gSrc participant)
        coinductiveFullProjectableFixtures

    it "[PROJ-CF-002] coinductive-full rejects incompatible send label-sets under merge" $
      expectProjectionFails
        projectCoinductiveFull
        gSendConflict
        "p"

    -- Hint completion bug: global rec binders leaked into local graphs,
    -- causing crashes (c) and spurious rec wrappers (a, b, d).
    it "[PROJ-HINT-001] coinductive-full projects hint-bug fixture onto c" $
      expectProjectionAs
        projectCoinductiveFull
        gHintBug
        "c"
        "rec t1 . d ! {m: a ? {m: t1}, m': a ? {m: rec t5 . a ? {m: t5}}}"

    it "[PROJ-HINT-002] coinductive-full projection onto a has no spurious recs" $
      expectProjectionAs
        projectCoinductiveFull
        gHintBug
        "a"
        "b ! {m: rec t4 . c ! {m: b ! {m: t4, m': rec t3 . c ! {m: t3}}}, m': rec t1 . c ! {m: t1}}"

    it "[PROJ-HINT-003] coinductive-full projection onto b has no spurious recs" $
      expectProjectionAs
        projectCoinductiveFull
        gHintBug
        "b"
        "a ? {m: rec t1 . a ? {m: t1, m': end}, m': end}"

    it "[PROJ-HINT-004] coinductive-full projection onto d has no spurious recs" $
      expectProjectionAs
        projectCoinductiveFull
        gHintBug
        "d"
        "rec t1 . c ? {m: t1, m': end}"

    it "[PROJ-CF-003] coinductive-full projects the same global onto q" $
      expectProjectionAs
        projectCoinductiveFull
        gSendConflict
        "q"
        "p ? {l1: end, l2: end}"

gIp :: String
gIp =
  "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l1: t } }"

gIf :: String
gIf =
  "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l2: end } }"

gItp23 :: String
gItp23 =
  "rec t . Alice -> Bob { string: rec t2 . Carl -> Dave { left: t, right: Alice -> Bob { string: t2 } } }"

gRing :: String
gRing =
  "a -> b { "
    ++ "AppThenGet: b -> c { AppThenGet: c -> a { Val: end } }, "
    ++ "App: b -> c { App: a -> c { Get: c -> a { Val: end } } } "
    ++ "}"

gOAuth :: String
gOAuth =
  "s -> c { "
    ++ "login: c -> a { passwd: a -> s { auth: end } }, "
    ++ "cancel: c -> a { quit: end } "
    ++ "}"

-- Global type with nested recursion where global rec binders are
-- irrelevant to some participants' local cycle structure.
gHintBug :: String
gHintBug =
  "rec t . a -> b { "
    ++ "m: c -> d { "
    ++ "m: a -> c { m: t }, "
    ++ "m': rec t2 . a -> c { m: a -> b { m: t2, m': rec t3 . a -> c { m: t3 } } } "
    ++ "}, "
    ++ "m': rec t4 . c -> d { "
    ++ "m: a -> c { m: t4 }, "
    ++ "m': rec t5 . a -> c { m: t5 } "
    ++ "} "
    ++ "}"

gSendConflict :: String
gSendConflict =
  "r -> s {a: p -> q {l1: end}, b: p -> q {l2: end}}"

coinductiveFullProjectableFixtures :: [(String, String)]
coinductiveFullProjectableFixtures =
  [ ("r -> s {a: q -> p {l1: end}, b: q -> p {l1: end, l2: end}}", "p")
  , (gSendConflict, "q")
  , ("rec t . r -> s {x: p -> q {l: t}}", "p")
  , ("p -> q {k: r -> s {x: q -> p {ack: end}, y: q -> p {ack: end}}}", "p")
  , (gIp, "p")
  , (gIf, "p")
  , (gItp23, "Alice")
  , (gRing, "c")
  , (gOAuth, "a")
  ]

expectProjectionAs ::
  (GlobalGraph -> Participant -> ProjectionResult) ->
  String ->
  String ->
  String ->
  Expectation
expectProjectionAs projectFn globalSrc participantName expectedLocalSrc =
  case parseGlobalTypeChecked globalSrc of
    Left err ->
      expectationFailure
        ( "Global type parse/check failed:\n"
            ++ err
        )
    Right globalType ->
      case projectFn (buildGlobalGraph globalType) (Participant participantName) of
        Left projectErr ->
          expectationFailure
            ( "Projection failed unexpectedly with: "
                ++ show projectErr
            )
        Right localGraph ->
          case localGraphToType localGraph of
            Left reconErr ->
              expectationFailure
                ( "Projection produced invalid local graph (cannot reconstruct local type): "
                    ++ show reconErr
                )
            Right projectedType ->
              case parseLocalTypeChecked expectedLocalSrc of
                Left expectedErr ->
                  expectationFailure
                    ( "Expected local type parse/check failed:\n"
                        ++ expectedErr
                    )
                Right expectedType ->
                  if sameLocalType projectedType expectedType
                    then pure ()
                    else
                      expectationFailure
                        ( "Projected type mismatch.\nExpected: "
                            ++ show expectedType
                            ++ "\nActual: "
                            ++ show projectedType
                        )

expectProjectionFails ::
  (GlobalGraph -> Participant -> ProjectionResult) ->
  String ->
  String ->
  Expectation
expectProjectionFails projectFn globalSrc participantName =
  case parseGlobalTypeChecked globalSrc of
    Left err ->
      expectationFailure
        ( "Global type parse/check failed:\n"
            ++ err
        )
    Right globalType ->
      case projectFn (buildGlobalGraph globalType) (Participant participantName) of
        Left _ -> pure ()
        Right localGraph ->
          expectationFailure
            ( "Projection unexpectedly succeeded with local graph: "
                ++ show localGraph
            )

expectProjectionSucceeds ::
  (GlobalGraph -> Participant -> ProjectionResult) ->
  String ->
  String ->
  Expectation
expectProjectionSucceeds projectFn globalSrc participantName =
  case parseGlobalTypeChecked globalSrc of
    Left err ->
      expectationFailure
        ( "Global type parse/check failed:\n"
            ++ err
        )
    Right globalType ->
      case projectFn (buildGlobalGraph globalType) (Participant participantName) of
        Left projectErr ->
          expectationFailure
            ( "Projection failed unexpectedly with: "
                ++ show projectErr
            )
        Right _ ->
          pure ()

sameLocalType :: LocalType -> LocalType -> Bool
sameLocalType actual expected =
  alphaEqLocalType
    (normalizeLocalBranchOrder actual)
    (normalizeLocalBranchOrder expected)
