module Main where

import System.IO
import System.Exit
import System.Environment
import Text.Printf
import Data.Bool
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

instrs :: String -> String
instrs str = 
  let
    input = Input str 0
    parsed_either =  sourceFileParser.run input
    parsed = either (\err -> error $ show_err err str) (fst) $ parsed_either
    compiled = printInstrs $ sourceCompiler parsed
  in
    compiled

parse :: String -> String
parse str = 
  let
    input = Input str 0
    parsed_either =  sourceFileParser.run input
    parsed = either (\err -> error $ show_err err str) (fst) $ parsed_either
  in
    fold $ map (print_exp 0) parsed

src_to_asm :: String -> String
src_to_asm input = initial_part ++ is ++ final_part
  where
    is = instrs input
    initial_part = "global _start\nextern printi\n\nsection .text\n\n"
    final_part = "_start:\n\tpush rbp\n\tmov rbp, rsp\n\n\tcall main\n\n\tpop rbp\n\n\tmov rax, 60\n\tmov rdi, 0\n\tsyscall\n"  

dumpASMOfFile :: FilePath -> IO()
dumpASMOfFile filepath = do
  str <- readFile filepath
  putStr $ src_to_asm str

parseFile :: FilePath -> IO()
parseFile filepath = do
  str <- readFile filepath
  putStr $ parse str

runFile :: FilePath -> IO ()
runFile filepath = do
  str <- readFile filepath
  let is = src_to_asm str
  handle <- openFile "compilation_out/main.asm" WriteMode
  hPutStr handle is
  hFlush handle
  hClose handle
  let process = (shell "make -B -s && ./main") {cwd = Just "compilation_out/"}
  (_, stdout_handle, _, proc_handle) <- createProcess process
  -- threadDelay 10000
  forkIO $ do
    threadDelay 1000000
    terminateProcess proc_handle
  waitForProcess proc_handle
  nullfile <- openFile "/dev/null" ReadMode
  output <- hGetContents $ maybe nullfile id stdout_handle
  putStr output
  -- removeFile "compilation_out/main"
  -- removeFile "compilation_out/main.asm"
  -- removeFile "compilation_out/main.o"
  
main :: IO()
main = do
  args <- getArgs
  let file = head args
  runFile file
  -- return ()
  -- processArgs args
