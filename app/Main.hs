module Main where

import System.IO
import System.Environment
import qualified System.Console.Readline as RL
import Text.Printf
import Data.Bool

import Lexer
import Parser
import Interpreter

process_text :: String -> String
process_text text = printf "tokens --> %s\nparse tree --> \n%s\ninterpreted value --> %s\n"
                    (show tokens) parse_tree_showed (show interpreted)
  where
    tokens = getTokens text
    
    parse_tree = parseSource tokens
    parse_tree_showed = print_exp 0 parse_tree
    interpreted = interpret_statement parse_tree

repl :: IO()
repl = do
  maybeLine <- RL.readline "% "
  case maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just "exit" -> return ()
    Just line -> do
      let output = process_text line
      putStr output
      repl

processArgs :: [String] -> IO ()
processArgs args
  | length args == 0 = error "need at least one subcommand out of repl or interpreter"
  | args !! 0 == "repl" = repl
  | args !! 0 == "run" = do
      fileHandle <- openFile (args !! 1) ReadMode
      contents <- hGetContents fileHandle
      putStr $ process_text contents
  | True = error "invalid subcommand"

main :: IO()
main = do
  args <- getArgs
  processArgs args
