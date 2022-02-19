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

import Data.List.Split

parse :: FilePath -> String -> IO [Expression]
parse input_filepath str = do
  let input = Input (preprocess str) 0
  let parsed_either =  sourceFileParser.run input
  let parsed = either (\err -> error $ show_err err str) (fst) $ parsed_either
  imported <- replace_imports parsed
  return imported
    where
      get_import_filepath :: String -> IO String
      get_import_filepath import_filepath = do
        input_abs <- makeAbsolute input_filepath
        import_filepath <- makeAbsolute $ ((concat $ intersperse "/" $ init $ splitOn "/" input_abs) ++ "/" ++ import_filepath)
        import_filepath_exists <- doesFileExist import_filepath
        case import_filepath_exists of
          True -> return import_filepath
          False -> error $ "import file " ++ import_filepath ++ " does not exist"

      replace_imports :: [Expression] -> IO [Expression]
      replace_imports [] = return []
      replace_imports ((Exp_Import filepath):rest) = do
        import_filepath <- get_import_filepath filepath
        file_contents <- readFile import_filepath
        exprs <- parse import_filepath file_contents
        rest_imported <- replace_imports rest
        return $ exprs ++ rest_imported
      replace_imports (e:rest) = do
        rest_imported <- replace_imports rest
        return $ e:rest_imported

bssify :: (String, String) -> String
bssify (str, name) = name ++ ": db " ++ (concat $ intersperse ", " $ (map (show . ord)) (str ++ "\0")) ++ "\n"

src_to_asm :: FilePath -> String -> IO String
src_to_asm filepath input_str = do
  parsed <- parse filepath input_str
  let (instructions, bss, procs) = sourceCompiler parsed
  let instructions_printed = printInstrs instructions
  let start_proc = "_start:\n\tpush rbp\n\tmov rbp, rsp\n\n\tcall main\n\n\tcall flush_out\n\tpop rbp\n\n\tmov rax, 60\n\tmov rdi, 0\n\tsyscall\n"  
  return $ "global _start\n" ++
    (fold $ map (\s -> "extern " ++ s ++ "\n") procs) ++
    "section .data\n" ++
    (fold $ map bssify bss) ++
    "section .text\n" ++
    instructions_printed ++
    start_proc

dumpASMOfFile :: FilePath -> IO()
dumpASMOfFile filepath = do
  str <- readFile filepath
  src_to_asm filepath str >>= putStr

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
  is <- src_to_asm filepath str
  handle <- openFile "compilation_out/main.asm" WriteMode
  hPutStr handle is
  hFlush handle
  hClose handle
  run_proc "nasm -f elf64 -g main.asm -o main.o"
  run_proc "gcc -c -o util.o util.c"
  run_proc "ld -g -o main main.o util.o -lc -dynamic-linker /lib64/ld-linux-x86-64.so.2"
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
