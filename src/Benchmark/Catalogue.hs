module Benchmark.Catalogue
  ( Citation(..)
  , GlobalExample(..)
  , LocalExample(..)
  , ParsedGlobalExample(..)
  , ParsedLocalExample(..)
  , globalExamples
  , localExamples
  , parseGlobalExample
  , parseLocalExample
  ) where

import Benchmark.Generators
  ( mkBinCounter
  , mkCcfGlobal
  , mkCcfLocal
  , mkCcfSimple
  , mkIndependentWorkers
  , mkIndependentWorkersGlobal
  , mkMapReduce
  , mkMapReduceGlobal
  )
import Benchmark.QBF (mkQBF, qbfGame)
import Benchmark.Types

-- ---------------------------------------------------------------------------
-- Citations
-- ---------------------------------------------------------------------------

scalasYoshida2019 :: Citation
scalasYoshida2019 = Citation "Scalas2019" "Scalas \\& Yoshida, 2019" Nothing

udomYoshida2025 :: Citation
udomYoshida2025 = Citation "thien-nobuko-popl-25" "Udomsrirungruang \\& Yoshida, 2025" Nothing

tiroreEtAl2023 :: Citation
tiroreEtAl2023 = Citation "Tirore2023" "Tirore et al., 2023" Nothing

castroPerezEtAl2026 :: Citation
castroPerezEtAl2026 = Citation "Castro2026" "Castro-Perez et al., 2026" Nothing

liEtAl2023 :: Citation
liEtAl2023 = Citation "Li2023" "Li et al., 2023" Nothing

hondaEtAl2016 :: Citation
hondaEtAl2016 = Citation "Honda2016" "Honda, Yoshida \\& Carbone, 2016" Nothing

yoshidaGheri2020 :: Citation
yoshidaGheri2020 = Citation "YoshidaGheri2020" "Yoshida \\& Gheri, 2020" Nothing

-- ---------------------------------------------------------------------------
-- Global examples (have a global type for projection benchmarks)
-- ---------------------------------------------------------------------------

globalExamples :: [GlobalExample]
globalExamples =
  [ gSimpleTravelAgency
  , gBetterTravelAgency
  , gOAuth
  , gTwoBuyer
  , gGIp
  , gGIf
  , gGItp23
  , gGRing
  , gGOddEven
  , gInstrControl
  , mkMapReduceGlobal 3
  , mkMapReduceGlobal 4
  , mkMapReduceGlobal 5
  , mkIndependentWorkersGlobal 2
  , mkIndependentWorkersGlobal 3
  , mkCcfGlobal 2
  , mkCcfGlobal 3
  , mkCcfGlobal 4
  , mkCcfGlobal 5
  ]

-- | G_sta Simple Travel Agency (Fig 1(a) from A Very Gentle Introduction to MPST)
gSimpleTravelAgency :: GlobalExample
gSimpleTravelAgency = GlobalExample
  { geName = "G_sta"
  , geCitation = Just yoshidaGheri2020 { citeRef = Just "Fig.~1(a)" }
  , geGlobalSource =
      "c -> a [string]; a -> c [int]; c -> a { "
        ++ "accept: c -> a [string]; a -> c [int]; end, "
        ++ "reject: end "
        ++ "}"
  , geParticipantNames = ["c", "a"]
  }

-- | G_bta Better Travel Agency (Fig 1(b) from A Very Gentle Introduction to MPST)
gBetterTravelAgency :: GlobalExample
gBetterTravelAgency = GlobalExample
  { geName = "G_bta"
  , geCitation = Just yoshidaGheri2020 { citeRef = Just "Fig.~1(b)" }
  , geGlobalSource =
      "rec t . c -> a [string]; a -> c [int]; c -> a { "
        ++ "accept: c -> a [string]; a -> c [int]; end, "
        ++ "retry: t, "
        ++ "reject: end "
        ++ "}"
  , geParticipantNames = ["c", "a"]
  }

-- | G_oa OAuth (Example 1 from Less Is More, 2019)
gOAuth :: GlobalExample
gOAuth = GlobalExample
  { geName = "G_oa"
  , geCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~1" }
  , geGlobalSource =
      "s -> c { "
        ++ "login: c -> a { password: c -> a [string]; a -> s { auth: a -> s [bool]; end } }, "
        ++ "auth: c -> a { quit: end } "
        ++ "}"
  , geParticipantNames = ["s", "c", "a"]
  }

-- | G_tb Recursive Two-Buyer (Example 2 from Less Is More, 2019)
gTwoBuyer :: GlobalExample
gTwoBuyer = GlobalExample
  { geName = "G_tb"
  , geCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~2" }
  , geGlobalSource =
      "rec t . a -> s [string]; s -> a [int]; "
        ++ "a -> b { "
        ++ "split: b -> a { yes: a -> s { buy: end }, no: t }, "
        ++ "cancel: a -> s { no: end } "
        ++ "}"
  , geParticipantNames = ["a", "s", "b"]
  }

-- | G_ip (Example 4.8 from POPL 2025)
gGIp :: GlobalExample
gGIp = GlobalExample
  { geName = "G_ip"
  , geCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , geGlobalSource =
      "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l1: t } }"
  , geParticipantNames = ["q", "r", "p"]
  }

-- | G_if (Example 4.8 from POPL 2025)
gGIf :: GlobalExample
gGIf = GlobalExample
  { geName = "G_if"
  , geCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , geGlobalSource =
      "rec t . q -> r { l1: r -> p { l1: t }, l2: r -> p { l2: end } }"
  , geParticipantNames = ["q", "r", "p"]
  }

-- | G_itp (ITP 2023)
gGItp23 :: GlobalExample
gGItp23 = GlobalExample
  { geName = "G_itp"
  , geCitation = Just tiroreEtAl2023
  , geGlobalSource =
      "rec t . a -> b [string]; rec t2 . c -> d { left: t, right: a -> b [string]; t2 }"
  , geParticipantNames = ["a", "b", "c", "d"]
  }

-- | G_ring (Ring protocol from POPL 2026)
gGRing :: GlobalExample
gGRing = GlobalExample
  { geName = "G_ring"
  , geCitation = Just castroPerezEtAl2026
  , geGlobalSource =
      "a -> b { "
        ++ "AppThenGet: b -> c { AppThenGet: c -> a { Val: end } }, "
        ++ "App: b -> c { App: a -> c { Get: c -> a { Val: end } } } "
        ++ "}"
  , geParticipantNames = ["a", "b", "c"]
  }

-- | G_oe "odd-even" (Example 2.1 from Li et al., CAV 2023)
gGOddEven :: GlobalExample
gGOddEven = GlobalExample
  { geName = "G_oe"
  , geCitation = Just liEtAl2023 { citeRef = Just "Ex.~2.1" }
  , geGlobalSource =
      "p -> q { "
        ++ "o : q -> r { o : rec t1 . p -> q { "
          ++ "o : q -> r { o : q -> r { o : t1 } }, "
          ++ "b : q -> r { b : r -> p { o : end } } "
        ++ "} }, "
        ++ "m : rec t2 . p -> q { "
          ++ "o : q -> r { o : q -> r { o : t2 } }, "
          ++ "b : q -> r { b : r -> p { m : end } } "
        ++ "} "
        ++ "}"
  , geParticipantNames = ["p", "q", "r"]
  }

-- | Instrument controlling (Example 3.10 from Honda, Yoshida & Carbone, 2016)
gInstrControl :: GlobalExample
gInstrControl = GlobalExample
  { geName = "InstrControl"
  , geCitation = Just hondaEtAl2016 { citeRef = Just "Ex.~3.10" }
  , geGlobalSource =
      "User -> Op [int]; "
        ++ "Op -> User { "
        ++ "ok: rec t . User -> Instr { "
          ++ "move: t, "
          ++ "photo: t, "
          ++ "quit: Instr -> Op [string]; end "
        ++ "}, "
        ++ "no: end "
        ++ "}"
  , geParticipantNames = ["User", "Op", "Instr"]
  }

-- ---------------------------------------------------------------------------
-- Local examples (have local type context for synthesis benchmarks)
-- ---------------------------------------------------------------------------

localExamples :: [LocalExample]
localExamples =
  -- Classic examples
  [ lTwoBuyers
  , lMapReduce5
  , mkMapReduce 4
  , mkMapReduce 5
  , lOAuth
  , mkIndependentWorkers 2
  , mkIndependentWorkers 3
  , lGIp
  , lGIf
  , lGItp23
  , lGRing
  , lGOddEven
  , lInstrControl
  -- Delta examples (local only)
  , lDelta5
  , lDelta6
  , lDelta7
  , lDelta8
  , lDelta9
  -- G_cf family
  , mkCcfLocal 2
  , mkCcfLocal 3
  , mkCcfLocal 4
  , mkCcfLocal 5
  -- G_cfs family
  , mkCcfSimple 2
  , mkCcfSimple 3
  , mkCcfSimple 4
  , mkCcfSimple 5
  -- Binary counter family
  , mkBinCounter 2
  , mkBinCounter 3
  , mkBinCounter 4
  , mkBinCounter 5
  -- QBF encoding
  , mkQBF qbfGame
  ]

-- | G_tb Recursive Two-Buyer (Example 2 from Less Is More, 2019)
lTwoBuyers :: LocalExample
lTwoBuyers = LocalExample
  { leName = "G_tb"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~2" }
  , leParticipants =
      [ ( "a"
        , "rec t . s ! [string]; s ? [int]; b ! { split: b ? { yes: s ! { buy: end }, no: t }, cancel: s ! { no: end } }"
        )
      , ( "s"
        , "rec t . a ? [string]; a ! [int]; a ? { buy: end, no: end }"
        )
      , ( "b"
        , "rec t . a ? { cancel: end, split: a ! { no: t, yes: end } }"
        )
      ]
  }

-- | MapReduce with 5 participants (Example 3 from Less Is More, 2019)
lMapReduce5 :: LocalExample
lMapReduce5 = LocalExample
  { leName = "G_mr-5"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~3" }
  , leParticipants =
      [ ( "m"
        , "rec t . w1 ! { datum: w1 ! [int]; w2 ! { datum: w2 ! [int]; w3 ! { datum: w3 ! [int]; "
          ++ "r ? { continue: r ? [int]; t, stop: w1 ! { stop: w2 ! { stop: w3 ! { stop: end } } } } } } }"
        )
      , ( "w1"
        , "m ? { datum: rec t . m ? [int]; r ! [int]; m ? { datum: t, stop: end } }"
        )
      , ( "w2"
        , "m ? { datum: rec t . m ? [int]; r ! [int]; m ? { datum: t, stop: end } }"
        )
      , ( "w3"
        , "m ? { datum: rec t . m ? [int]; r ! [int]; m ? { datum: t, stop: end } }"
        )
      , ( "r"
        , "rec t . w1 ? [int]; w2 ? [int]; w3 ? [int]; m ! { continue: m ! [int]; t, stop: end }"
        )
      ]
  }

-- | G_oa OAuth (Example 1 from Less Is More, 2019)
lOAuth :: LocalExample
lOAuth = LocalExample
  { leName = "G_oa"
  , leCitation = Just scalasYoshida2019 { citeRef = Just "Ex.~1" }
  , leParticipants =
      [ ( "s", "c ! {auth: end, login: a ? {auth: a ? [bool]; end}}" )
      , ( "c", "s ? {auth: a ! {quit: end}, login: a ! {password: a ! [string]; end}}" )
      , ( "a", "c ? {password: c ? [string]; s ! {auth: s ! [bool]; end}, quit: end}" )
      ]
  }

-- | G_ip (Example 4.8 from POPL 2025)
lGIp :: LocalExample
lGIp = LocalExample
  { leName = "G_ip"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , leParticipants =
      [ ( "q", "rec t . r ! { l1: t, l2: t }" )
      , ( "r", "rec t . q ? {l1: p ! {l1: t}, l2: p ! {l1: t}}" )
      , ( "p", "rec t . r ? {l1: t}" )
      ]
  }

-- | G_if (Example 4.8 from POPL 2025)
lGIf :: LocalExample
lGIf = LocalExample
  { leName = "G_if"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Ex.~4.8" }
  , leParticipants =
      [ ( "q", "rec t . r ! {l1: rec t1 . r ! {l1: t1, l2: end}, l2: end}" )
      , ( "r", "rec t . q ? {l1: p ! {l1: t}, l2: p ! {l2: end}}" )
      , ( "p", "rec t . r ? {l1: t, l2: end}" )
      ]
  }

-- | G_itp (ITP 2023)
lGItp23 :: LocalExample
lGItp23 = LocalExample
  { leName = "G_itp"
  , leCitation = Just tiroreEtAl2023
  , leParticipants =
      [ ( "a", "rec t . b ! [string]; t" )
      , ( "b", "rec t . a ? [string]; t" )
      , ( "c", "rec t . rec t2 . d ! { left: t, right: t2 }" )
      , ( "d", "rec t . rec t2 . c ? { left: t, right: t2 }" )
      ]
  }

-- | G_ring (Ring protocol from POPL 2026)
lGRing :: LocalExample
lGRing = LocalExample
  { leName = "G_ring"
  , leCitation = Just castroPerezEtAl2026
  , leParticipants =
      [ ( "a", "b ! {App: c ! {Get: c ? {Val: end}}, AppThenGet: c ? {Val: end}}" )
      , ( "b", "a ? {App: c ! {App: end}, AppThenGet: c ! {AppThenGet: end}}" )
      , ( "c", "b ? {App: a ? {Get: a ! {Val: end}}, AppThenGet: a ! {Val: end}}" )
      ]
  }

-- | G_oe "odd-even" (Example 2.1 from Li et al., CAV 2023)
lGOddEven :: LocalExample
lGOddEven = LocalExample
  { leName = "G_oe"
  , leCitation = Just liEtAl2023 { citeRef = Just "Ex.~2.1" }
  , leParticipants =
      [ ( "p"
        , "q ! { o : rec t1 . q ! { o : t1, b : r ? { o : end } }, "
          ++ "m : rec t2 . q ! { o : t2, b : r ? { m : end } } }"
        )
      , ( "q"
        , "p ? { o : r ! { o : rec t1 . p ? { o : r ! { o : r ! { o : t1 } }, b : r ! { b : end } } }, "
          ++ "m : rec t2 . p ? { o : r ! { o : r ! { o : t2 } }, b : r ! { b : end } } }"
        )
      , ( "r"
        , "rec t . q ? { o : q ? { o : t }, b : p ! { o : end, m : end } }"
        )
      ]
  }

-- | Instrument controlling (Example 3.10 from Honda, Yoshida & Carbone, 2016)
lInstrControl :: LocalExample
lInstrControl = LocalExample
  { leName = "InstrControl"
  , leCitation = Just hondaEtAl2016 { citeRef = Just "Ex.~3.10" }
  , leParticipants =
      [ ( "User", "Op ! [int]; Op ? { ok: rec t . Instr ! { move: t, photo: t, quit: end }, no: end }" )
      , ( "Op", "User ? [int]; User ! { ok: Instr ? [string]; end, no: end }" )
      , ( "Instr", "rec t . User ? { move: t, photo: t, quit: Op ! [string]; end }" )
      ]
  }

-- | Delta5 (POPL 2025 Fig 4)
lDelta5 :: LocalExample
lDelta5 = LocalExample
  { leName = "Delta5"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "p ? { l1: r ? { l2: end, l3: end }, l4: r ? { l2: end, l5: end } }" )
      , ( "p", "q ! { l1: end, l4: end }" )
      , ( "r", "q ! { l2: end }" )
      ]
  }

-- | Delta6 (POPL 2025 Fig 4)
lDelta6 :: LocalExample
lDelta6 = LocalExample
  { leName = "Delta6"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "p ? { l1: end, l2: end }" )
      , ( "p", "q ! { l1: end, l3: end }" )
      ]
  }

-- | Delta7 (POPL 2025 Fig 4)
lDelta7 :: LocalExample
lDelta7 = LocalExample
  { leName = "Delta7"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "rec t . p ? { val_S: t }" )
      , ( "p", "rec t . q ! { val_S: t }" )
      , ( "r", "s ? { l2: end }" )
      , ( "s", "r ! { l1: end }" )
      , ( "u", "v ! { l1: end }" )
      ]
  }

-- | Delta8 (POPL 2025 Fig 4)
lDelta8 :: LocalExample
lDelta8 = LocalExample
  { leName = "Delta8"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "p ? { val_S: end }" )
      ]
  }

-- | Delta9 (POPL 2025 Fig 4)
lDelta9 :: LocalExample
lDelta9 = LocalExample
  { leName = "Delta9"
  , leCitation = Just udomYoshida2025 { citeRef = Just "Fig.~4" }
  , leParticipants =
      [ ( "q", "rec t . p ? { val_S: t }" )
      , ( "p", "rec t . q ! { val_S: t }" )
      , ( "r", "s ? { val_bool: end }" )
      ]
  }
