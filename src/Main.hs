module Main where

import System.IO
import System.Environment
import Text.Printf
import Data.Bool
import qualified Data.Map as Map
import qualified System.Process as Process

import Lexer
import Parser
import Interpreter
import Compiler

interpret_text :: String -> IO ()
interpret_text text = do
  putStr $ printf "tokens --> %s\nparse tree --> \n%s\ninterpreted value --> %s\n"
                    (show tokens) parse_tree_showed (show interpreted)
  -- compile_text_and_output_and_call_compilation text
  where
    tokens = getTokens text
    
    parse_tree = parseSource tokens
    parse_tree_showed = print_exp 0 parse_tree
    (symtab, interpreted) = interpret_block Map.empty parse_tree

compile_text_and_output_and_call_compilation :: String -> IO ()
compile_text_and_output_and_call_compilation text = do
  writeFile "output.asm" asm_folded
  -- putStr "\n--- asm ---\n"
  -- putStr asm_folded
  -- createProcess $ proc "nasm" ["nasm", "-f elf64", "-o output.o", "output.asm"]
  where
    tokens = getTokens text
    
    parse_tree = parseSource tokens
    parse_tree_showed = print_exp 0 parse_tree
    asm = exprToASM parse_tree Map.empty 0 0 "rax"
    asm_folded = foldl (++) "" asm

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
        "exit" -> return ()
        "" -> repl
        _ -> do
          interpret_text line
          repl

processArgs :: [String] -> IO ()
processArgs args
  | length args == 0 = putStrLn "need at least one subcommand out of repl or interpreter"
  | args !! 0 == "repl" = repl
  | args !! 0 == "run" = do
      fileHandle <- openFile (args !! 1) ReadMode
      contents <- hGetContents fileHandle
      interpret_text contents
  | True = error "invalid subcommand"

main :: IO()
main = do
  args <- getArgs
  processArgs args
