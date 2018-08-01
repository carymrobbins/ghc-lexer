{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.GHC.Parser.Main where

import Language.Haskell.GHC.Parser.Internal.JSON

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char
import Data.IORef
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe
import System.Console.Haskeline
import System.Directory
import System.IO
import System.IO.Unsafe
import System.Environment
import Text.Read (readMaybe)

import DynFlags
import FastString
import GHC
import GHC.Paths
import GHC.LanguageExtensions (Extension)
import qualified GHC.LanguageExtensions
import Lexer
import SrcLoc
import StringBuffer

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  runInputT myHaskelineSettings loop
  where
  loop :: InputT IO ()
  loop = getInputLine _PROMPT >>= \case
    Nothing -> return ()
    Just c -> runCommand c

  runCommand c = case break (== ' ') c of
    ("", "") -> loop
    ("lex", argStr) -> withParseArgsIO argStr $ lexCmd runLazyLexer
    ("lexall", argStr) -> withParseArgsIO argStr $ lexCmd runStrictLexer
    (q, _) | isQuit q -> return ()
    (cmd, _) -> do
      outputStrLn $ "Unknown command: " ++ cmd
      loop

  isQuit q = q `elem` ["quit", "q", "exit"]

  lexCmd f args = case argsFile args of
    Nothing -> liftIO (lexStdin f args) >> loop
    Just file -> do
      x <- liftIO $ doesFileExist file
      if x then liftIO (lexFile f file args) else outputStrLn $ "File not found: " ++ file
      loop

  lexFile f file args = do
    contents <- readFile file
    let stringBuf =
            stringToStringBuffer
          $ intercalate "\n"
          $ map applyCommentCPPToLine
          $ splitOn "\n" contents
    f stringBuf args

  lexStdin f args = do
    stringBuf <- consumeStdin
    f stringBuf args

data Args = Args
  { argsFile :: Maybe FilePath
  , argsExts :: [Extension]
  }

withParseArgsIO :: MonadIO io => String -> (Args -> io ()) -> io ()
withParseArgsIO argStr f = either (liftIO . putStrLn) f $ parseArgs argStr

parseArgs :: String -> Either String Args
parseArgs argStr = loop argStr $ Args Nothing []
  where
  loop s args = case s of
    c : rest | isSpace c -> loop rest args
    c : rest | c `elem` ['"', '\''] -> parseFile (Just c) rest args
    '-' : 'X' : rest -> parseExt rest args
    "" -> Right args
    rest -> parseFile Nothing rest args

  parseFile maybeQuote s args = case maybeQuote of
    Just q -> case break (== q) s of
      (file, "") -> Left $ "Unterminated string: " ++ (q : file)
      (file, _ : rest) -> continueWithArgsFile args file rest
    Nothing -> case break isSpace s of
      (file, rest) -> continueWithArgsFile args file rest

  continueWithArgsFile args file rest = case argsFile args of
    Just _ -> Left $ "File already supplied, unexpected argument: " ++ file
    Nothing | null file -> Left "Invalid file: empty string"
    Nothing -> loop rest args { argsFile = Just file }

  parseExt s args = case readMaybe s of
    Nothing -> Left $ "Invalid extension: " ++ s
    Just x -> Right args { argsExts = x : argsExts args }

myHaskelineSettings :: System.Console.Haskeline.Settings IO
myHaskelineSettings = defaultSettings { historyFile = Just $ homeDir ++ "/.ghc-lexer-history" }

decodeLangPragma :: Located Token -> [Extension]
decodeLangPragma (L _ tok) = case tok of
  ITblockComment s -> decode s
  _ -> []
  where
  decode s =
    if isLangPragma then mapMaybe readMaybe extStrings else []
    where
    isLangPragma :: Bool
    isLangPragma = any (`isPrefixOf` map toLower s) ["{-# language ", "{-#language"]

    extStrings :: [String]
    extStrings =
        map (filter (/= ','))
      $ filter (\x -> map toLower x `notElem` ["{-#", "language", "{-#language", "#-}"])
      $ words s

-- Standalone deriving instance so we can easily decode Extensions from Strings.
deriving instance Read Extension

{-# NOINLINE globalDynFlags #-}
globalDynFlags :: DynFlags
globalDynFlags = unsafePerformIO $ runGhc (Just libdir) $ return unsafeGlobalDynFlags

defaultFlags :: DynFlags
defaultFlags = foldl gopt_set globalDynFlags [Opt_KeepRawTokenStream, Opt_Haddock]

initSrcLoc :: RealSrcLoc
initSrcLoc = mkRealSrcLoc (mkFastString "a.hs") 1 1

runLazyLexer :: StringBuffer -> Args -> IO ()
runLazyLexer stringBuf args = do
  putStrLn "OUTPUT: (Press enter for next element. Anything else will stop lexing)"
  runLexer (("" ==) <$> getLine) stringBuf args

runStrictLexer :: StringBuffer -> Args -> IO ()
runStrictLexer = runLexer (return True)

runLexer :: IO Bool -> StringBuffer -> Args -> IO ()
runLexer shouldContinue stringBuf Args {..} = do
  pStateRef <- newIORef initialState
  loop pStateRef
  where
  allExts = argsExts ++ obtainExtensions stringBuf

  initialFlags = updateDynFlagExtensions allExts defaultFlags

  initialState = mkPState initialFlags stringBuf initSrcLoc

  loop pStateRef = shouldContinue >>= \case
    True  -> showNextToken pStateRef
    False -> return ()

  showNextToken pStateRef = do
    pState <- readIORef pStateRef
    case unP (lexer False return) pState of
      PFailed srcSpan msgDoc -> putJSONLn $ Failure msgDoc srcSpan
      POk pState' tok -> do
        writeIORef pStateRef pState'
        -- writeIORef pStateRef $ adjustState pState' tok
        putJSONLn $ locTokenToJSON tok
        case tok of
          L _ ITeof -> return ()
          L _ _     -> loop pStateRef

obtainExtensions :: StringBuffer -> [Extension]
obtainExtensions stringBuf =
  case lexTokenStream stringBuf initSrcLoc defaultFlags of
    POk _ tokens -> tokens >>= decodeLangPragma
    _ -> []

updateDynFlagExtensions :: [Extension] -> DynFlags -> DynFlags
updateDynFlagExtensions exts dflags = foldl xopt_set dflags exts

{-# NOINLINE _STDIN_EOF #-}
_STDIN_EOF :: String
_STDIN_EOF = fromMaybe "<EOF>" $ unsafePerformIO $ lookupEnv "STDIN_EOF"

{-# NOINLINE _PROMPT #-}
_PROMPT :: String
_PROMPT = fromMaybe "> " $ unsafePerformIO $ lookupEnv "PROMPT"

{-# NOINLINE _COMMENT_CPP #-}
_COMMENT_CPP :: Bool
_COMMENT_CPP = case unsafePerformIO (lookupEnv "COMMENT_CPP") of
  Nothing -> defaultValue
  Just s  ->
    case map toLower s of
      ""      -> defaultValue
      "1"     -> True
      "true"  -> True
      "0"     -> False
      "false" -> False
      _ -> error $ "Unexpected COMMENT_CPP value: " ++ show s
  where
  defaultValue = False

{-# NOINLINE homeDir #-}
homeDir :: String
homeDir = unsafePerformIO getHomeDirectory

applyCommentCPPToLine :: String -> String
applyCommentCPPToLine line = case line of
  '#':_:rest | _COMMENT_CPP -> "--" ++ rest
  _                         -> line

consumeStdin :: IO StringBuffer
consumeStdin = do
  putStrLn $ "INPUT: (Enter code to parse. Use " ++ show _STDIN_EOF ++ " on a line to signal end of file.)"
  contents <- getLines
  return $ stringToStringBuffer contents
  where
  getOneLine :: IO String
  getOneLine = applyCommentCPPToLine <$> getLine

  getLinesWhile :: (String -> Bool) -> IO String
  getLinesWhile p = unlines <$> takeWhileM p (repeat getOneLine)

  getLines :: IO String
  getLines = getLinesWhile (/= _STDIN_EOF)

  takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
  takeWhileM p (ma : mas) = do
      a <- ma
      if p a
        then (a :) <$> takeWhileM p mas
        else return []
  takeWhileM _ _ = return []

-- Adapted from Hakyll.Core.Util.String
trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
  trim' = dropWhile isSpace
