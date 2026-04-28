module Main where

import Automata
  ( GlobalGraph(..)
  , GlobalEdgeLabel(..)
  , GlobalPayloadEdgeLabel(..)
  , globalPayloadOutgoing
  )
import MPST
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetContents, hPutStrLn, stderr, stdin)
import Visualise

import Control.Monad (when)
import Data.List (intercalate, nub, sortOn)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    []             -> hPutStrLn stderr usage >> exitFailure
    ["--help"]     -> putStrLn usage >> exitSuccess
    ["-h"]         -> putStrLn usage >> exitSuccess
    ["help"]       -> putStrLn usage >> exitSuccess
    (cmd : rest)   -> dispatch cmd rest

dispatch :: String -> [String] -> IO ()
dispatch cmd args = case cmd of
  "parse-global"  -> cmdParseGlobal args
  "parse-local"   -> cmdParseLocal args
  "parse-process" -> cmdParseProcess args
  "render-global" -> cmdRenderGlobal args
  "render-local"  -> cmdRenderLocal args
  "project"       -> cmdProject args
  "balanced"      -> cmdBalanced args
  "check"         -> cmdCheck args
  "synthesise"    -> cmdSynthesise args
  "synthesize"    -> cmdSynthesise args
  "subtype"       -> cmdSubtype args
  "typecheck"     -> cmdTypecheck args
  "infer"         -> cmdInfer args
  _               -> die ("Unknown command: " ++ cmd ++ "\n\n" ++ usage)

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

cmdParseGlobal :: [String] -> IO ()
cmdParseGlobal [src] = do
  g <- parseOrDie parseGlobalTypeChecked "global type" src
  putStrLn (renderGlobalType g)
cmdParseGlobal _ = die "Usage: mpst parse-global \"<global-type>\""

cmdParseLocal :: [String] -> IO ()
cmdParseLocal [src] = do
  l <- parseOrDie parseLocalTypeChecked "local type" src
  putStrLn (renderLocalType l)
cmdParseLocal _ = die "Usage: mpst parse-local \"<local-type>\""

cmdParseProcess :: [String] -> IO ()
cmdParseProcess [src] = do
  p <- parseOrDie parseProcessChecked "process" src
  putStrLn (renderProcess p)
cmdParseProcess _ = die "Usage: mpst parse-process \"<process>\""

cmdRenderGlobal :: [String] -> IO ()
cmdRenderGlobal [src, out] = do
  g <- parseOrDie parseGlobalTypeChecked "global type" src
  let graph = buildGlobalGraph g
  _ <- renderGlobalPng out graph
  hPutStrLn stderr ("Wrote " ++ out)
cmdRenderGlobal _ = die "Usage: mpst render-global \"<global-type>\" OUT.png"

cmdRenderLocal :: [String] -> IO ()
cmdRenderLocal [src, out] = do
  l <- parseOrDie parseLocalTypeChecked "local type" src
  let graph = buildLocalGraph l
  _ <- renderLocalPng out graph
  hPutStrLn stderr ("Wrote " ++ out)
cmdRenderLocal _ = die "Usage: mpst render-local \"<local-type>\" OUT.png"

cmdProject :: [String] -> IO ()
cmdProject args = do
  let (variant, role, positionals) = parseProjectArgs args
      projFn = case variant of
        Nothing   -> projectCoinductiveFull
        Just "ip" -> projectInductivePlain
        Just "if" -> projectInductiveFull
        Just "cp" -> projectCoinductivePlain
        Just "cf" -> projectCoinductiveFull
        Just v    -> error ("Unknown projection variant: " ++ v
                           ++ " (use ip, if, cp, cf)")
  case positionals of
    [src] -> do
      g <- parseOrDie parseGlobalTypeChecked "global type" src
      let gg = buildGlobalGraph g
          participants = extractParticipants gg
          targets = case role of
            Just r  -> [Participant r]
            Nothing -> participants
      if null targets
        then die "No participants found in global type."
        else mapM_ (projectOne projFn gg) targets
    _ -> die "Usage: mpst project [--variant ip|if|cp|cf] [--role NAME] \"<global-type>\""

projectOne :: (GlobalGraph -> Participant -> ProjectionResult) -> GlobalGraph -> Participant -> IO ()
projectOne projFn gg p@(Participant name) =
  case projFn gg p of
    Left err -> hPutStrLn stderr (name ++ ": projection error: " ++ show err)
    Right lg -> case localGraphToType lg of
      Left err -> hPutStrLn stderr (name ++ ": graph-to-type error: " ++ show err)
      Right lt -> putStrLn (name ++ ": " ++ renderLocalType lt)

parseProjectArgs :: [String] -> (Maybe String, Maybe String, [String])
parseProjectArgs = go Nothing Nothing []
  where
    go v r pos [] = (v, r, reverse pos)
    go _ r pos ("--variant" : val : rest) = go (Just val) r pos rest
    go v _ pos ("--role" : val : rest)    = go v (Just val) pos rest
    go v r pos (arg : rest)               = go v r (arg : pos) rest

cmdCheck :: [String] -> IO ()
cmdCheck args = do
  src <- getFileInput args
  ctx <- parseContext src
  let localGraphs = Map.map buildLocalGraph ctx
      cg = buildContextGraph (Map.toList localGraphs)
      safeRes = checkSafety cg
      dfRes   = checkDeadlockFreedom cg
      liveRes = checkLiveness cg
  putStrLn $ "Participants:     " ++ intercalate ", " (map getParticipant (Map.keys ctx))
  putStrLn $ "Safety:           " ++ renderResult safeRes
  putStrLn $ "Deadlock freedom: " ++ renderResult dfRes
  putStrLn $ "Liveness:         " ++ renderResult liveRes
  case (safeRes, dfRes, liveRes) of
    (Right (), Right (), Right ()) -> exitSuccess
    _ -> exitFailure

cmdSynthesise :: [String] -> IO ()
cmdSynthesise args = do
  src <- getFileInput args
  ctx <- parseContext src
  let localGraphs = Map.map buildLocalGraph ctx
      cg = buildContextGraph (Map.toList localGraphs)
  case synthesise cg of
    Left err -> die ("Synthesis error: " ++ show err)
    Right gg -> case globalGraphToType gg of
      Left err -> die ("Graph-to-type error: " ++ show err)
      Right gt -> putStrLn (renderGlobalType gt)

cmdSubtype :: [String] -> IO ()
cmdSubtype [src1, src2] = do
  l1 <- parseOrDie parseLocalTypeChecked "local type (left)" src1
  l2 <- parseOrDie parseLocalTypeChecked "local type (right)" src2
  let lg1 = buildLocalGraph l1
      lg2 = buildLocalGraph l2
  case checkLocalSubtype lg1 lg2 of
    Right () -> putStrLn "Subtype: yes" >> exitSuccess
    Left errs -> do
      putStrLn "Subtype: no"
      mapM_ (putStrLn . ("  " ++) . show) errs
      exitFailure
cmdSubtype _ = die "Usage: mpst subtype \"<local-type-1>\" \"<local-type-2>\""

cmdTypecheck :: [String] -> IO ()
cmdTypecheck [procSrc, typeSrc] = do
  p <- parseOrDie parseProcessChecked "process" procSrc
  l <- parseOrDie parseLocalTypeChecked "local type" typeSrc
  case typecheck p l of
    Right () -> putStrLn "Typecheck: ok" >> exitSuccess
    Left errs -> do
      putStrLn "Typecheck: failed"
      mapM_ (putStrLn . ("  " ++) . show) errs
      exitFailure
cmdTypecheck _ = die "Usage: mpst typecheck \"<process>\" \"<local-type>\""

cmdInfer :: [String] -> IO ()
cmdInfer [src] = do
  p <- parseOrDie parseProcessChecked "process" src
  case infer p of
    Right lt -> putStrLn (renderLocalType lt)
    Left errs -> do
      hPutStrLn stderr "Inference failed:"
      mapM_ (hPutStrLn stderr . ("  " ++) . show) errs
      exitFailure
cmdInfer _ = die "Usage: mpst infer \"<process>\""

cmdBalanced :: [String] -> IO ()
cmdBalanced [src] = do
  g <- parseOrDie parseGlobalTypeChecked "global type" src
  let gg = buildGlobalGraph g
      bal  = checkBalanced gg
      wbal = checkWeakBalanced gg
  putStrLn $ "Balanced:      " ++ renderResult bal
  putStrLn $ "Weak balanced: " ++ renderResult wbal
  case (bal, wbal) of
    (Right (), _) -> exitSuccess
    (_, Right ()) -> exitSuccess
    _             -> exitFailure
cmdBalanced _ = die "Usage: mpst balanced \"<global-type>\""

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseOrDie :: (String -> Either String a) -> String -> String -> IO a
parseOrDie parser what src =
  case parser src of
    Left err -> die ("Invalid " ++ what ++ ":\n" ++ err)
    Right x  -> pure x

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

renderResult :: Either [a] () -> String
renderResult (Right ()) = "yes"
renderResult (Left errs) = "no (" ++ show (length errs) ++ " violation(s))"

-- | Read input from a file path, or stdin if the path is "-" or omitted.
getFileInput :: [String] -> IO String
getFileInput ["-"]  = hGetContents stdin
getFileInput [path] = readFile path
getFileInput []     = hGetContents stdin
getFileInput _      = die "Expected a single file argument, or - for stdin."

-- | Parse a context file: lines of "participant: local-type".
-- Blank lines and lines starting with '#' are ignored.
parseContext :: String -> IO (Map.Map Participant LocalType)
parseContext src = do
  let ls = filter (not . isBlankOrComment) (lines src)
  when (null ls) $ die "Empty context (no participant lines found)."
  pairs <- mapM parseLine ls
  pure (Map.fromList pairs)
  where
    isBlankOrComment s =
      let stripped = dropWhile (== ' ') s
      in null stripped || head stripped == '#'
    parseLine line =
      case break (== ':') line of
        (_, []) -> die ("Invalid context line (expected 'participant: type'):\n  " ++ line)
        (name, _ : rest) -> do
          let pName = trim name
              tSrc  = trim rest
          lt <- parseOrDie parseLocalTypeChecked ("local type for " ++ pName) tSrc
          pure (Participant pName, lt)
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Extract all participant names from a global graph's edges.
extractParticipants :: GlobalGraph -> [Participant]
extractParticipants gg =
  let edgeParts = concatMap (\(_, labels) ->
        concatMap (\l -> [geSender l, geReceiver l]) labels)
        (Map.toList (ggEdgeLabels gg))
      payloadParts = concatMap (\(_, labels) ->
        concatMap (\(l, _) -> [gpeSender l, gpeReceiver l]) labels)
        (Map.toList (globalPayloadOutgoing gg))
  in nub (sortOn getParticipant (edgeParts ++ payloadParts))

-- ---------------------------------------------------------------------------
-- Usage
-- ---------------------------------------------------------------------------

usage :: String
usage = unlines
  [ "mpst - Multiparty Session Types tool"
  , ""
  , "Usage: mpst <command> [options] [input]"
  , ""
  , "Commands:"
  , "  parse-global \"<global-type>\"           Parse and pretty-print a global type"
  , "  parse-local \"<local-type>\"             Parse and pretty-print a local type"
  , "  parse-process \"<process>\"               Parse and pretty-print a process"
  , "  render-global \"<global-type>\" OUT.png   Render global automaton as PNG"
  , "  render-local \"<local-type>\" OUT.png     Render local automaton as PNG"
  , ""
  , "  project [--variant ip|if|cp|cf] [--role NAME] \"<global-type>\""
  , "      Project a global type onto participant(s)."
  , "      Defaults to coinductive full (cf) and all participants."
  , "      Variants: ip = inductive plain, if = inductive full,"
  , "                cp = coinductive plain, cf = coinductive full."
  , ""
  , "  balanced \"<global-type>\""
  , "      Check balancedness (strong and weak) of a global type."
  , ""
  , "  check <context-file>"
  , "      Check safety, deadlock freedom, and liveness of a context."
  , "      Context file format: one 'participant: local-type' per line."
  , "      Lines starting with '#' are comments."
  , ""
  , "  synthesise <context-file>"
  , "      Synthesise a global type from a context."
  , ""
  , "  subtype \"<local-type-1>\" \"<local-type-2>\""
  , "      Check whether the first local type is a subtype of the second."
  , ""
  , "  typecheck \"<process>\" \"<local-type>\""
  , "      Typecheck a process against a local type."
  , ""
  , "  infer \"<process>\""
  , "      Infer a local type from a process."
  , ""
  , "Options:"
  , "  -h, --help    Show this help"
  ]
