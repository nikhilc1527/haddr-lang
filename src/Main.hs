module Main where

import System.IO
import System.Environment
import Text.Printf
import Data.Bool
import qualified Data.Map as Map
import qualified System.Process as Process
import qualified Debug.Trace as Trace

import System.IO
import System.Process
import System.Directory
import Control.Concurrent
import Control.Monad.State

import Lexer
import Parser2
import Interpreter2
import Compiler

dumpASM :: String -> IO()
dumpASM str = do
  let is = instrs str
  let asm = initial_part ++ is ++ final_part
  putStr asm
  where
    instrs = printInstrs . ((flip evalState) $ CompilerState 0 Map.empty 0) . compile . either (const Exp_Empty) (fst) . (parseSource :: Parser Char (Error Char String) Expression).run . (flip Input) 0
    initial_part = "global main\nextern printi\n\nsection .text\n\nmain:\n\tpush rbp\n\tmov rbp, rsp\n\n"
    final_part = "\n\tmov rdi, rax\n\tcall printi\n\n\tpop rbp\n\n\tmov rax, 0\n\tret\n"
parseAndCompileExp :: String -> IO ()
parseAndCompileExp str = do
  let is = instrs str
  handle <- openFile "compilation_out/main.asm" WriteMode
  hPutStr handle initial_part
  hPutStr handle is
  hPutStr handle final_part
  hFlush handle
  let process = (shell "make -B -s && ./main") {cwd = Just "compilation_out/"}
  (_, stdout_handle, _, proc_handle) <- createProcess process
  -- threadDelay 10000
  forkIO $ do
    threadDelay 1000000
    terminateProcess proc_handle
  waitForProcess proc_handle
  null <- openFile "/dev/null" ReadMode
  output <- hGetContents $ maybe null id stdout_handle
  putStr output
  removeFile "compilation_out/main"
  removeFile "compilation_out/main.asm"
  removeFile "compilation_out/main.o"
  where
    instrs = printInstrs . ((flip evalState) $ CompilerState 0 Map.empty 0) . compile . either (const Exp_Empty) (fst) . (parseSource :: Parser Char (Error Char String) Expression).run . (flip Input) 0
    initial_part = "global main\nextern printi\n\nsection .text\n\nmain:\n\tpush rbp\n\tmov rbp, rsp\n\n"
    final_part = "\n\tmov rdi, rax\n\tcall printi\n\n\tpop rbp\n\n\tmov rax, 0\n\tret\n"

interpret_text :: String -> IO ()
interpret_text text = return ()
  -- do
  -- putStr $ printf "interpreted value --> %s\n" (show interpreted)
  -- putStr $ printf "tokens --> %s\nparse tree --> \n%s\ninterpreted value --> %s\n"
  --                   (show tokens) parse_tree_showed (show interpreted)
  -- compile_text_and_output_and_call_compilation text
  where
    a = 1
    -- tokens = getTokens text
    
    -- parse_tree = parseSource tokens
    -- parse_tree_showed = print_exp 0 parse_tree
    -- !(symtab, interpreted) = interpret_block initialSymbolTable parse_tree

compile_text_and_output_and_call_compilation :: String -> IO ()
compile_text_and_output_and_call_compilation text = undefined
  -- do
  -- writeFile "output.asm" asm_folded
  -- putStr "\n--- asm ---\n"
  -- putStr asm_folded
  -- createProcess $ proc "nasm" ["nasm", "-f elf64", "-o output.o", "output.asm"]
  where
    a = 0
    -- tokens = getTokens text
    
    -- parse_tree = parseSource tokens
    -- parse_tree_showed = print_exp 0 parse_tree
    -- asm = exprToASM parse_tree Map.empty 0 0 "rax"
    -- asm_folded = foldl (++) "" asm

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
