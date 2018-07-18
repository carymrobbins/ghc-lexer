module Language.Haskell.GHC.Parser.Main where

import Language.Haskell.GHC.Parser.Internal.JSON

import Control.Monad.Trans (liftIO)
import Data.Char
import Data.IORef
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe
import System.Console.Haskeline
import System.Directory
import System.IO
import System.IO.Unsafe

import DynFlags
import FastString
import GHC
import GHC.Paths
import Lexer
import SrcLoc
import StringBuffer
import System.Environment

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
    ("", "")          -> loop
    ("lex", arg)      -> lexCmd runLazyLexer arg
    ("lexall", arg)   -> lexCmd runStrictLexer arg
    (q, _) | isQuit q -> return ()
    (cmd, _) -> do
      outputStrLn $ "Unknown command: " ++ cmd
      loop

  isQuit q = q `elem` ["quit", "q", "exit"]

  lexCmd f arg = case trim arg of
    ""   -> liftIO (lexStdin f) >> loop
    file -> do
      x <- liftIO $ doesFileExist file
      if x then liftIO (lexFile f file) else outputStrLn $ "File not found: " ++ file
      loop

  lexFile f file = do
    contents <- readFile file
    let stringBuf =
            stringToStringBuffer
          $ intercalate "\n"
          $ map applyCommentCPPToLine
          $ splitOn "\n" contents
    f stringBuf

  lexStdin f = do
    stringBuf <- consumeStdin
    f stringBuf

myHaskelineSettings :: System.Console.Haskeline.Settings IO
myHaskelineSettings = defaultSettings { historyFile = Just $ homeDir ++ "/.ghc-lexer-history" }

{-# NOINLINE globalDynFlags #-}
globalDynFlags :: DynFlags
globalDynFlags = unsafePerformIO $ runGhc (Just libdir) $ return unsafeGlobalDynFlags

defaultFlags :: DynFlags
defaultFlags =
    flip gopt_set Opt_KeepRawTokenStream
  . flip gopt_set Opt_Haddock
  $ globalDynFlags

initSrcLoc :: RealSrcLoc
initSrcLoc = mkRealSrcLoc (mkFastString "a.hs") 1 1

runLazyLexer :: StringBuffer -> IO ()
runLazyLexer stringBuf = do
  putStrLn "OUTPUT: (Press enter for next element. Anything else will stop lexing)"
  runLexer (("" ==) <$> getLine) stringBuf

runStrictLexer :: StringBuffer -> IO ()
runStrictLexer = runLexer (return True)

runLexer :: IO Bool -> StringBuffer -> IO ()
runLexer shouldContinue stringBuf = do
  let initialState = mkPState defaultFlags stringBuf initSrcLoc
  pStateRef <- newIORef initialState
  loop pStateRef
  where
  loop pStateRef = shouldContinue >>= \case
    True  -> showNextToken pStateRef
    False -> return ()

  showNextToken pStateRef = do
    pState <- readIORef pStateRef
    case unP (lexer False return) pState of
      PFailed srcSpan msgDoc -> putJSONLn $ Failure msgDoc srcSpan
      POk pState' tok -> do
        writeIORef pStateRef pState'
        putJSONLn $ locTokenToJSON tok
        case tok of
          L _ ITeof -> return ()
          L _ _     -> loop pStateRef

{-# NOINLINE _STDIN_EOF #-}
_STDIN_EOF :: String
_STDIN_EOF = fromMaybe "<EOF>" $ unsafePerformIO $ lookupEnv "STDIN_EOF"

{-# NOINLINE _PROMPT #-}
_PROMPT :: String
_PROMPT = fromMaybe "> " $ unsafePerformIO $ lookupEnv "PROMPT"

{-# NOINLINE _COMMENT_CPP #-}
_COMMENT_CPP :: Bool
_COMMENT_CPP = case unsafePerformIO (lookupEnv "COMMENT_CPP") of
  Nothing -> True
  Just s  ->
    case map toLower s of
      ""      -> True
      "1"     -> True
      "true"  -> True
      "0"     -> False
      "false" -> False
      _ -> error $ "Unexpected COMMENT_CPP value: " ++ show s

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
