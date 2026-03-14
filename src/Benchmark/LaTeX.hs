module Benchmark.LaTeX
  ( renderGlobalTable
  , renderLocalTable
  , renderLaTeXDocument
  ) where

import Benchmark.Types (Citation(..))
import Benchmark.Runner
  ( GlobalBenchResult(..)
  , LocalBenchResult(..)
  , ProjVariantResult(..)
  )
import Data.Char (isDigit)
import Data.List (intercalate, stripPrefix)
import Data.Time.Clock (NominalDiffTime)

-- ---------------------------------------------------------------------------
-- Global table
-- ---------------------------------------------------------------------------

-- | Render the global benchmarks table.
renderGlobalTable :: Int -> [GlobalBenchResult] -> String
renderGlobalTable numRuns results = unlines
  [ "\\begin{table}[t]"
  , "\\centering"
  , "\\caption{Top-down projection benchmarks: median times over $" ++ show numRuns ++ "$ runs."
  , "  IP = inductive plain, IF = inductive full,"
  , "  CP = coinductive plain, CF = coinductive full.}"
  , "\\label{tab:global-benchmarks}"
  , "\\resizebox{\\textwidth}{!}{"
  , "\\scriptsize"
  , "\\begin{tabular}{l l r r l r r r r r}"
  , "\\toprule"
  , "Example & Citation & $|G|$ & $|\\textsf{pt}(G)|$"
    ++ " & $\\textsf{Bal}(G)$ & $|\\Delta_{\\textsf{pro}}|$"
    ++ " & IP & IF & CP & CF \\\\"
  , "\\midrule"
  , intercalate "\n" (map renderGlobalRow results)
  , "\\bottomrule"
  , "\\end{tabular}"
  , "}"
  , "\\end{table}"
  ]

renderGlobalRow :: GlobalBenchResult -> String
renderGlobalRow br =
  let name = formatNameLaTeX (gbrName br)
      cite = formatCite (gbrCitation br)
      gSize = show (gbrGlobalSize br)
      nPart = show (gbrNumParticipants br)
      bal = (if gbrBalanced br then "$\\checkmark$" else "$\\times$")
            ++ " " ++ formatTime (gbrBalancedTime br)
      projSize = maybe "---" show (gbrProjectedSize br)
      ip  = fmtVariant (gbrIP br)
      ifu = fmtVariant (gbrIF br)
      cp  = fmtVariant (gbrCP br)
      cf  = fmtVariant (gbrCF br)
   in intercalate " & " [name, cite, gSize, nPart, bal, projSize, ip, ifu, cp, cf]
        ++ " \\\\"

-- ---------------------------------------------------------------------------
-- Local table
-- ---------------------------------------------------------------------------

-- | Render the local benchmarks table.
renderLocalTable :: Int -> [LocalBenchResult] -> String
renderLocalTable numRuns results = unlines
  [ "\\begin{table}[t]"
  , "\\centering"
  , "\\caption{Bottom-up synthesis benchmarks: median times over $" ++ show numRuns ++ "$ runs.}"
  , "\\label{tab:local-benchmarks}"
  , "\\resizebox{\\textwidth}{!}{"
  , "\\scriptsize"
  , "\\begin{tabular}{l l r r c c r r c r}"
  , "\\toprule"
  , "Example & Citation & $|\\Delta|$ & $|\\textsf{dom}(\\Delta)|$"
    ++ " & $\\textsf{safe}$ & $\\textsf{live}$"
    ++ " & mpstk & $|G_{\\textsf{inf}}|$ & $\\textsf{Bal}(G_{\\textsf{inf}})$ & Synth. \\\\"
  , "\\midrule"
  , intercalate "\n" (map renderLocalRow results)
  , "\\bottomrule"
  , "\\end{tabular}"
  , "}"
  , "\\end{table}"
  ]

renderLocalRow :: LocalBenchResult -> String
renderLocalRow br =
  let name = formatNameLaTeX (lbrName br)
      cite = formatCite (lbrCitation br)
      ctxSize = show (lbrContextSize br)
      nPart = show (lbrNumParticipants br)
      safe = fmtBool (lbrSafe br)
      live = fmtBool (lbrLive br)
      mpstk = maybe "---" formatTime (lbrMpstkTime br)
      infSize = maybe "---" show (lbrInferredSize br)
      infBal = fmtBool (lbrInferredBalanced br)
      synth = maybe "---" formatTime (lbrSynthesisTime br)
   in intercalate " & " [name, cite, ctxSize, nPart, safe, live, mpstk, infSize, infBal, synth]
        ++ " \\\\"

-- ---------------------------------------------------------------------------
-- Standalone document
-- ---------------------------------------------------------------------------

-- | Render a standalone compilable LaTeX document with both tables.
renderLaTeXDocument :: Int -> [GlobalBenchResult] -> [LocalBenchResult] -> String
renderLaTeXDocument numRuns globalResults localResults = unlines
  [ "\\documentclass{article}"
  , "\\usepackage{booktabs}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{amssymb}"
  , "\\usepackage{graphicx}"
  , "\\begin{document}"
  , ""
  , renderGlobalTable numRuns globalResults
  , ""
  , renderLocalTable numRuns localResults
  , ""
  , "\\end{document}"
  ]

-- ---------------------------------------------------------------------------
-- Formatting helpers
-- ---------------------------------------------------------------------------

-- | Format a citation with optional example reference.
formatCite :: Maybe Citation -> String
formatCite Nothing = "---"
formatCite (Just c) = case citeRef c of
  Nothing  -> "\\cite{" ++ citeKey c ++ "}"
  Just ref -> "\\cite[" ++ ref ++ "]{" ++ citeKey c ++ "}"

-- | Format a projection variant: time if ok, ✗ if failed, --- if timed out.
fmtVariant :: Maybe ProjVariantResult -> String
fmtVariant Nothing = "---"
fmtVariant (Just pv)
  | pvAllOk pv = formatTime (pvTime pv)
  | otherwise   = "$\\times$"

-- | Format a boolean result.
fmtBool :: Maybe Bool -> String
fmtBool Nothing      = "---"
fmtBool (Just True)  = "$\\checkmark$"
fmtBool (Just False) = "$\\times$"

-- | Format a time duration adaptively.
formatTime :: NominalDiffTime -> String
formatTime dt
  | us < 1000    = showFixed 1 us ++ "\\,\\textmu s"
  | us < 1000000 = showFixed 1 (us / 1000) ++ "\\,ms"
  | otherwise     = showFixed 2 (us / 1000000) ++ "\\,s"
  where
    us = realToFrac dt * 1000000 :: Double

-- | Show a Double with a fixed number of decimal places.
showFixed :: Int -> Double -> String
showFixed n x =
  let factor = 10 ^ n :: Int
      rounded = fromIntegral (round (x * fromIntegral factor) :: Int) / fromIntegral factor :: Double
   in showFFloat' n rounded

showFFloat' :: Int -> Double -> String
showFFloat' n x =
  let intPart = truncate x :: Int
      fracPart = abs (round ((x - fromIntegral intPart) * fromIntegral (10 ^ n :: Int)) :: Int)
      fracStr = padLeft n '0' (show fracPart)
   in show intPart ++ "." ++ fracStr

padLeft :: Int -> Char -> String -> String
padLeft n c s
  | length s >= n = s
  | otherwise     = replicate (n - length s) c ++ s

-- | Format a benchmark name for LaTeX using math-mode macros.
--
-- Naming conventions:
--   G_xxx       → $\G[xxx]$                      (global type examples)
--   DeltaN      → $\ctx[N]$                       (context examples)
--   C_cf-N      → $C_{\textsf{cf}}(N)$             (conflict-free context families)
--   C_cfs-N     → $C_{\textsf{cfs}}(N)$            (simplified conflict-free)
--   BinCtr-N    → $\textsf{BinCtr}(N)$             (binary counter)
--   MapReduce-N → $\textsf{MapReduce}(N)$          (map-reduce)
--   other       → escaped plain text
formatNameLaTeX :: String -> String
formatNameLaTeX name
  | Just sub <- stripPrefix "G_" name =
      "$\\G[" ++ sub ++ "]$"
  | Just digits <- stripPrefix "Delta" name, all isDigit digits, not (null digits) =
      "$\\ctx[" ++ digits ++ "]$"
  | Just rest <- stripPrefix "C_cfs-" name =
      "$C_{\\textsf{cfs}}(" ++ rest ++ ")$"
  | Just rest <- stripPrefix "C_cf-" name =
      "$C_{\\textsf{cf}}(" ++ rest ++ ")$"
  | Just rest <- stripPrefix "BinCtr-" name =
      "$\\textsf{BinCtr}(" ++ rest ++ ")$"
  | Just rest <- stripPrefix "MapReduce-" name =
      "$\\textsf{MapReduce}(" ++ rest ++ ")$"
  | otherwise = escapeLaTeX name

-- | Escape special LaTeX characters.
escapeLaTeX :: String -> String
escapeLaTeX = concatMap escapeChar
  where
    escapeChar '_' = "\\_"
    escapeChar '&' = "\\&"
    escapeChar '%' = "\\%"
    escapeChar '#' = "\\#"
    escapeChar c   = [c]
