{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Benchmark.Catalogue (globalExamples, localExamples, parseGlobalExample, parseLocalExample)
import Benchmark.LaTeX (renderGlobalTable, renderLocalTable, renderLaTeXDocument)
import Benchmark.Runner
  ( GlobalBenchResult(..)
  , LocalBenchResult(..)
  , ProjVariantResult(..)
  , runGlobalBenchmarks
  , runLocalBenchmarks
  , runSingleGlobalBenchmark
  , runSingleLocalBenchmark
  )
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem)
import Control.Exception (SomeException, catch)
import Data.Time.Clock (NominalDiffTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hPutStrLn, hSetBuffering, stderr)
import System.Process (readProcess)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let config = parseArgs args defaultConfig

  -- Auto-detect mpstk availability
  mpstkAvailable <- if cfgNoMpstk config
    then pure False
    else detectMpstk

  hPutStrLn stderr "Configuration:"
  hPutStrLn stderr $ "  Runs per benchmark: " ++ show (cfgRuns config)
  hPutStrLn stderr $ "  mpstk available:    " ++ show mpstkAvailable
  hPutStrLn stderr $ "  Timeout (s):        " ++ show (cfgTimeout config)
  hPutStrLn stderr $ "  Parallel:           " ++ show (cfgParallel config)
  hPutStrLn stderr $ "  Output:             " ++ maybe "stdout" id (cfgOutput config)
  hPutStrLn stderr $ "  Standalone:         " ++ show (cfgStandalone config)
  hPutStrLn stderr ""

  -- Parse global examples
  let globalParseResults = map parseGlobalExample globalExamples
  parsedGlobals <- case sequence globalParseResults of
    Left err -> do
      hPutStrLn stderr $ "Global parse error: " ++ err
      exitFailure
    Right gs -> do
      hPutStrLn stderr $ "Parsed " ++ show (length gs) ++ " global examples."
      pure gs

  -- Parse local examples
  let localParseResults = map parseLocalExample localExamples
  parsedLocals <- case sequence localParseResults of
    Left err -> do
      hPutStrLn stderr $ "Local parse error: " ++ err
      exitFailure
    Right ls -> do
      hPutStrLn stderr $ "Parsed " ++ show (length ls) ++ " local examples."
      pure ls

  hPutStrLn stderr ""

  -- Create semaphore to limit concurrent mpstk (JVM) processes
  mpstkSem <- if mpstkAvailable && cfgParallel config
    then Just <$> newQSem 4
    else pure Nothing

  let nRuns     = cfgRuns config
      timeoutS  = cfgTimeout config

  -- Run global benchmarks
  hPutStrLn stderr "Running global benchmarks..."
  globalResults <- if cfgParallel config
    then mapConcurrently (runSingleGlobalBenchmark nRuns timeoutS) parsedGlobals
    else runGlobalBenchmarks nRuns timeoutS parsedGlobals

  -- Run local benchmarks
  hPutStrLn stderr "Running local benchmarks..."
  localResults <- if cfgParallel config
    then mapConcurrently (runSingleLocalBenchmark mpstkAvailable nRuns timeoutS mpstkSem) parsedLocals
    else runLocalBenchmarks mpstkAvailable nRuns timeoutS mpstkSem parsedLocals

  -- Print human-readable summary to stderr
  hPutStrLn stderr ""
  hPutStrLn stderr "=== Global Results ==="
  hPutStrLn stderr (renderGlobalSummary globalResults)
  hPutStrLn stderr "=== Local Results ==="
  hPutStrLn stderr (renderLocalSummary localResults)

  -- Output LaTeX
  let latex = if cfgStandalone config
        then renderLaTeXDocument nRuns globalResults localResults
        else renderGlobalTable nRuns globalResults ++ "\n" ++ renderLocalTable nRuns localResults
  case cfgOutput config of
    Nothing   -> putStr latex
    Just path -> writeFile path latex >> hPutStrLn stderr ("LaTeX written to " ++ path)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgNoMpstk    :: Bool
  , cfgRuns       :: Int
  , cfgTimeout    :: Int
  , cfgOutput     :: Maybe FilePath
  , cfgStandalone :: Bool
  , cfgParallel   :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgNoMpstk    = False
  , cfgRuns       = 10
  , cfgTimeout    = 60
  , cfgOutput     = Nothing
  , cfgStandalone = False
  , cfgParallel   = False
  }

parseArgs :: [String] -> Config -> Config
parseArgs [] cfg = cfg
parseArgs ("--no-mpstk" : rest) cfg = parseArgs rest cfg { cfgNoMpstk = True }
parseArgs ("--standalone" : rest) cfg = parseArgs rest cfg { cfgStandalone = True }
parseArgs ("--parallel" : rest) cfg = parseArgs rest cfg { cfgParallel = True }
parseArgs ("--runs" : n : rest) cfg = parseArgs rest cfg { cfgRuns = read n }
parseArgs ("--timeout" : n : rest) cfg = parseArgs rest cfg { cfgTimeout = read n }
parseArgs ("-t" : n : rest) cfg = parseArgs rest cfg { cfgTimeout = read n }
parseArgs ("-o" : path : rest) cfg = parseArgs rest cfg { cfgOutput = Just path }
parseArgs ("--output" : path : rest) cfg = parseArgs rest cfg { cfgOutput = Just path }
parseArgs (_ : rest) cfg = parseArgs rest cfg

detectMpstk :: IO Bool
detectMpstk = do
  (readProcess "mpstk" ["verify", "--help"] "" >> pure True)
    `catch` (\(_ :: SomeException) -> pure False)

-- ---------------------------------------------------------------------------
-- Human-readable summaries
-- ---------------------------------------------------------------------------

renderGlobalSummary :: [GlobalBenchResult] -> String
renderGlobalSummary results =
  let header = padR 13 "Example"
            ++ padR 6 "|G|"
            ++ padR 6 "|pt|"
            ++ padR 6 "Bal"
            ++ padR 10 "BalTime"
            ++ padR 8 "|D_p|"
            ++ padR 10 "IP"
            ++ padR 10 "IF"
            ++ padR 10 "CP"
            ++ padR 10 "CF"
      sep = replicate (length header) '-'
      rows = map globalSummaryRow results
   in unlines (header : sep : rows)

globalSummaryRow :: GlobalBenchResult -> String
globalSummaryRow br =
  padR 13 (gbrName br)
    ++ padR 6 (show (gbrGlobalSize br))
    ++ padR 6 (show (gbrNumParticipants br))
    ++ padR 6 (if gbrBalanced br then "yes" else "no")
    ++ padR 10 (formatTimeHuman (gbrBalancedTime br))
    ++ padR 8 (maybe "---" show (gbrProjectedSize br))
    ++ padR 10 (fmtVariantH (gbrIP br))
    ++ padR 10 (fmtVariantH (gbrIF br))
    ++ padR 10 (fmtVariantH (gbrCP br))
    ++ padR 10 (fmtVariantH (gbrCF br))

renderLocalSummary :: [LocalBenchResult] -> String
renderLocalSummary results =
  let header = padR 13 "Example"
            ++ padR 6 "|D|"
            ++ padR 6 "|dom|"
            ++ padR 6 "safe"
            ++ padR 6 "live"
            ++ padR 10 "mpstk"
            ++ padR 8 "|G_i|"
            ++ padR 6 "Bal"
            ++ padR 10 "Synth."
      sep = replicate (length header) '-'
      rows = map localSummaryRow results
   in unlines (header : sep : rows)

localSummaryRow :: LocalBenchResult -> String
localSummaryRow br =
  padR 13 (lbrName br)
    ++ padR 6 (show (lbrContextSize br))
    ++ padR 6 (show (lbrNumParticipants br))
    ++ padR 6 (fmtBoolH (lbrSafe br))
    ++ padR 6 (fmtBoolH (lbrLive br))
    ++ padR 10 (maybe "---" formatTimeHuman (lbrMpstkTime br))
    ++ padR 8 (maybe "---" show (lbrInferredSize br))
    ++ padR 6 (fmtBoolH (lbrInferredBalanced br))
    ++ padR 10 (maybe "---" formatTimeHuman (lbrSynthesisTime br))

fmtVariantH :: Maybe ProjVariantResult -> String
fmtVariantH Nothing = "---"
fmtVariantH (Just pv)
  | pvAllOk pv = formatTimeHuman (pvTime pv)
  | otherwise   = "fail"

fmtBoolH :: Maybe Bool -> String
fmtBoolH Nothing      = "---"
fmtBoolH (Just True)  = "yes"
fmtBoolH (Just False) = "no"

padR :: Int -> String -> String
padR n s
  | length s >= n = s
  | otherwise     = s ++ replicate (n - length s) ' '

formatTimeHuman :: NominalDiffTime -> String
formatTimeHuman dt
  | us < 1000    = showN 1 us ++ " us"
  | us < 1000000 = showN 1 (us / 1000) ++ " ms"
  | otherwise     = showN 2 (us / 1000000) ++ " s"
  where
    us = realToFrac dt * 1000000 :: Double

showN :: Int -> Double -> String
showN n x =
  let factor = 10 ^ n :: Int
      rounded = fromIntegral (round (x * fromIntegral factor) :: Int) / fromIntegral factor :: Double
      intPart = truncate rounded :: Int
      fracPart = abs (round ((rounded - fromIntegral intPart) * fromIntegral (10 ^ n :: Int)) :: Int)
      fracStr = let s = show fracPart in replicate (n - length s) '0' ++ s
   in show intPart ++ "." ++ fracStr
