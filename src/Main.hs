module Main where

import System.IO
import System.Environment
import Text.Printf
import Data.Bool
import qualified Data.Map as Map

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
    (symtab, interpreted) = interpret_block Map.empty parse_tree

-- if you want a nice repl experience use the readline library wrapper https://github.com/hanslub42/rlwrap
repl :: IO()
repl = do
  putStr "haddr >>> "
  hFlush stdout
  eof <- hIsEOF stdin
  if eof then putStrLn "\nexit"
    else do
      line <- getLine
      case line of
        "exit"      -> return ()
        "" -> repl
        _ -> do
          let output = process_text line
          putStr output
          repl

processArgs :: [String] -> IO ()
processArgs args
  | length args == 0 = putStrLn "need at least one subcommand out of repl or interpreter"
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
