{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.GHC.Parser.Main where

import Language.Haskell.GHC.Parser.Internal.JSON

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Aeson as J
import Data.Char
import Data.IORef
import qualified Data.Text.IO as T
import System.Exit
import System.IO
import System.IO.Unsafe
import Text.Read

import BasicTypes
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
  mFile <- getArgs >>= \case
    []  -> return Nothing
    [x] -> return $ Just x
    xs  -> error $ "Expected 0-1 arguments, got " ++ show (length xs)

  flags <- runGhc (Just libdir) $ return unsafeGlobalDynFlags

  hSetBuffering stdout LineBuffering

  case mFile of
    Nothing   -> runStdin flags
    Just file -> runFile file flags

  where
  runFile file flags = do
    stringBuf <- hGetStringBuffer file -- stringToStringBuffer <$> readFile file
    runLazyLexer stringBuf flags

  runStdin flags = do
    putStrLn "INPUT:"
    stringBuf <- consumeStdin
    runLazyLexer stringBuf flags
    runStdin flags

runLazyLexer stringBuf flags = do
  let srcLoc = mkRealSrcLoc (mkFastString "a.hs") 0 0
  let initialState = mkPState flags stringBuf srcLoc
  pStateRef <- newIORef initialState
  putStrLn "OUTPUT: (Press enter for next element. Anything else will exit immediately)"
  loop pStateRef
  where
  loop pStateRef = getLine >>= \case
    "" -> showNextToken pStateRef
    _  -> putStrLn "exiting..."

  showNextToken pStateRef = do
    pState <- readIORef pStateRef
    case unP (lexer False return) pState of
      POk pState' ltok -> do
        writeIORef pStateRef pState'
        case ltok of
          L _ ITeof -> putStrLn "EOF"
          l@(L _ _) -> do
            LC8.putStrLn $ J.encode $ locTokenToJSON l -- srcSpan ++ ": " ++ show token
            loop pStateRef

      PFailed srcSpan msgDoc -> do
        hPutStrLn stderr $ "Lexer failed: " ++ show srcSpan {- ++ ": " ++ show msgDoc -}
        -- exitFailure

  -- Can remove this when we write a ToJSON L instance.
--   mkJson srcSpan token =
--     "{\"srcSpan\":" <> show (toJSON srcSpan) <> ",\"token\":" <> show (toJSON token) <> "}"

{-# NOINLINE _STDIN_PARSE_SEP #-}
_STDIN_PARSE_SEP :: String
_STDIN_PARSE_SEP = case unsafePerformIO (lookupEnv "STDIN_PARSE_SEP") of
  Just s -> s
  Nothing -> "<<<STOP PARSE>>>"

consumeStdin :: IO StringBuffer
consumeStdin = do
  contents <- getLines
  return $ stringToStringBuffer contents
  where
  getLinesWhile :: (String -> Bool) -> IO String
  getLinesWhile p = fmap unlines $ takeWhileM p (repeat getLine)

  getLines :: IO String
  getLines = getLinesWhile (/= _STDIN_PARSE_SEP)

  takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
  takeWhileM p (ma : mas) = do
      a <- ma
      if p a
        then fmap (a :) $ takeWhileM p mas
        else return []
  takeWhileM _ _ = return []

--   withSystemTempFile "ghc-parser-stdin" $ \path _ -> do
-- --   writeTempFileFromStdin h
--   BS.hGetContents stdin >>= BS.writeFile path
--   hGetStringBuffer path
--   where
--   writeTempFileFromStdin h = do
--     bs <- BS.hGetLine stdin
--     -- Check for NUL byte
--     if not (BS.null bs) && BS.head bs == 0 then
--       return ()
--     else do
--       C8.hPutStrLn h bs
--       writeTempFileFromStdin h

-- consumeStdin :: IO StringBuffer
-- consumeStdin = readBlock >>= loop
--   where
--   blockSize = 1024
--
--   readBlock :: IO StringBuffer
--   readBlock = hGetStringBufferBlock stdin blockSize
--
--   loop :: StringBuffer -> IO StringBuffer
--   loop block = do
--     next <- readBlock
--     if len next == 0 then return block else appendStringBuffers block next >>= loop
