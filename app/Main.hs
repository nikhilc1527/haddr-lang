module Main where

import System.IO
import System.Environment
import qualified System.Console.Readline as RL
import Text.Printf
import Data.Bool

import Lexer
import Parser

process_text :: String -> String
process_text text = printf "tokens --> %s\nstatement --> %s\nparse tree --> \n%s\n" (show tokens) (show statements) parse_tree_showed
  where
    tokens = getTokens text
    statements = getStatements tokens
    (parse_tree, tokens2) = unwrap_maybe_exp $ parseL1 $ reverse $ head statements
    f x
      | x == '(' = "\n(\n"
      | x == ')' = "\n)"
      | True = [x]
    -- parse_tree_showed = unlines $ pretty_print 0 $ 
    --   lines $ concat $ map f $ show parse_tree
    parse_tree_showed = print_exp 0 parse_tree

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
