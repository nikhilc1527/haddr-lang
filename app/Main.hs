module Main where

import System.IO
import System.Environment
import qualified System.Console.Readline as RL

import Lexer
import Parser

repl :: IO()
repl = do
  maybeLine <- RL.readline "% "
  case maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just "exit" -> return ()
    Just line -> do
      let tokens = getTokens line
      print tokens
      repl

processArgs :: [String] -> IO ()
processArgs args
  | length args == 0 = error "need at least one subcommand out of repl or interpreter"
  | args !! 0 == "repl" = repl
  | args !! 0 == "run" = do
      fileHandle <- openFile (args !! 1) ReadMode
      contents <- hGetContents fileHandle
      let tokens = getTokens contents
      print tokens
  | True = error "invalid subcommand"

main :: IO()
main = do
  args <- getArgs
  processArgs args
