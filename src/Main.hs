module Main where

import System.IO
import System.Exit
import System.Environment
import Text.Printf
import Data.Bool
import Data.Char
import Data.List
import Data.Foldable
import qualified Data.Map as Map
import qualified System.Process as Process
import qualified Debug.Trace as Trace

import System.IO
import System.Process
import System.Directory
import Control.Concurrent
import Control.Monad.State

import Parser
import Compiler   

parse :: String -> String
parse str = 
  let
    input = Input str 0
    parsed_either =  sourceFileParser.run input
    parsed = either (\err -> error $ show_err err str) (fst) $ parsed_either
  in
    fold $ map (print_exp 0) parsed

bssify :: (String, String) -> String
bssify (str, name) = name ++ ": db " ++ (concat $ intersperse ", " $ (map (show . ord)) (str ++ "\0")) ++ "\n"

src_to_asm :: String -> String
src_to_asm input_str = 
  "global _start\n" ++
  (fold $ map (\s -> "extern " ++ s ++ "\n") procs) ++
  "section .data\n" ++
  (fold $ map bssify bss) ++
  "section .text\n" ++
  instructions_printed ++
  start_proc
  where
    uncommented = uncomment $ input_str
    input = Input uncommented 0
    parsed_either = sourceFileParser.run input
    parsed = either (\err -> error $ show_err err input_str) (fst) $ parsed_either
    (instructions, bss, procs) = sourceCompiler parsed
    instructions_printed = printInstrs instructions
    start_proc = "_start:\n\tpush rbp\n\tmov rbp, rsp\n\n\tcall main\n\n\tcall flush_out\n\tpop rbp\n\n\tmov rax, 60\n\tmov rdi, 0\n\tsyscall\n"  

dumpASMOfFile :: FilePath -> IO()
dumpASMOfFile filepath = do
  str <- readFile filepath
  putStr $ src_to_asm str

parseFile :: FilePath -> IO()
parseFile filepath = do
  str <- readFile filepath
  putStr $ parse str

runFileForever :: FilePath -> IO ()
runFileForever filepath = do
  compileFile filepath
  let process = (shell "./main") {cwd = Just "compilation_out/"}
  (_, stdout_handle, _, proc_handle) <- createProcess process
  waitForProcess proc_handle
  nullfile <- openFile "/dev/null" ReadMode
  output <- hGetContents $ maybe nullfile id stdout_handle
  putStr output
  removeFile "compilation_out/main"
  removeFile "compilation_out/main.asm"
  removeFile "compilation_out/main.o"

runFile :: FilePath -> IO ()
runFile filepath = do
  compileFile filepath
  let process = (shell "./main") {cwd = Just "compilation_out/"}
  (_, stdout_handle, _, proc_handle) <- createProcess process
  -- threadDelay 10000
  forkIO $ do
    threadDelay 1000000
    terminateProcess proc_handle
  waitForProcess proc_handle
  nullfile <- openFile "/dev/null" ReadMode
  output <- hGetContents $ maybe nullfile id stdout_handle
  putStr output
  removeFile "compilation_out/main"
  removeFile "compilation_out/main.asm"
  removeFile "compilation_out/main.o"

compileFile :: FilePath -> IO ()
compileFile filepath = do
  str <- readFile filepath
  let is = src_to_asm str
  handle <- openFile "compilation_out/main.asm" WriteMode
  hPutStr handle is
  hFlush handle
  hClose handle
  run_proc "nasm -f elf64 -g main.asm -o main.o"
  run_proc "gcc -c -o util.o util.c"
  run_proc "ld -o main main.o util.o -lc -dynamic-linker /lib64/ld-linux-x86-64.so.2"
    where
      run_proc :: String -> IO()
      run_proc cmd = do
        let process = (shell cmd) {cwd = Just "compilation_out/"}
        (_, stdout_handle, _, proc_handle) <- createProcess process
        ecode <- waitForProcess proc_handle
        case ecode of
          ExitSuccess -> return ()
          ExitFailure n -> error $ "command: $(" ++ cmd ++ ") failed with exit code: " ++ (show n)

main :: IO()
main = do
  args <- getArgs
  let file = head args
  compileFile file
  return ()
  -- return ()
  -- processArgs args
