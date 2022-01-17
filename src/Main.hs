module Main where

import System.IO
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

dumpASMOfExpression :: String -> IO()
dumpASMOfExpression str = do
  let is = instrs str
  let asm = initial_part ++ is ++ final_part
  putStr asm
  where
    instrs = printInstrs . ((flip evalState) $ initialCompilerState) . compile . either (\err -> error $ show err) (fst) . expressionP.run . (flip Input) 0
    initial_part = "global _start\nextern printi\n\nsection .text\n\n"
    final_part = "_start:\n\tpush rbp\n\tmov rbp, rsp\n\n\tcall main\n\tmov rdi, rax\n\tcall printi\n\n\tpop rbp\n\n\tmov rax, 60\n\tmov rdi, 0\n\tsyscall\n"

dumpASMOfFile :: FilePath -> IO()
dumpASMOfFile filepath = do
  str <- readFile filepath
  let is = instrs str
  let asm = initial_part ++ is ++ final_part
  putStr asm
  where
    instrs = fold . map (printInstrs . ((flip evalState) $ initialCompilerState) . compile) . either (\err -> error $ show err) (fst) . sourceFileParser.run . (flip Input) 0
    initial_part = "global _start\nextern printi\n\nsection .text\n\n"
    final_part = "_start:\n\tpush rbp\n\tmov rbp, rsp\n\n\tcall main\n\n\tpop rbp\n\n\tmov rax, 60\n\tmov rdi, 0\n\tsyscall\n"  

parseAndCompileExp :: String -> IO ()
parseAndCompileExp str = do
  let is = instrs str
  handle <- openFile "compilation_out/main.asm" WriteMode
  hPutStr handle initial_part
  hPutStr handle is
  hPutStr handle final_part
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
  removeFile "compilation_out/main"
  removeFile "compilation_out/main.asm"
  removeFile "compilation_out/main.o"
  where
    instrs = printInstrs . ((flip evalState) $ CompilerState 0 Map.empty 0) . compile . either (const Exp_Empty) (fst) . (expressionP).run . (flip Input) 0
    initial_part = "global main\nextern printi\n\nsection .text\n\nmain:\n\tpush rbp\n\tmov rbp, rsp\n\n"
    final_part = "\n\tmov rdi, rax\n\tcall printi\n\n\tpop rbp\n\n\tmov rax, 0\n\tret\n"

parseAndCompileFile :: FilePath -> IO ()
parseAndCompileFile filepath = do
  handle <- openFile filepath ReadMode
  str <- hGetContents handle
  let is = instrs str
  handle <- openFile "compilation_out/main.asm" WriteMode
  hPutStr handle initial_part
  hPutStr handle is
  hPutStr handle final_part
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
  where
    instrs = fold . map (printInstrs . ((flip evalState) $ initialCompilerState) . compile) . either (\err -> error $ show err) (fst) . sourceFileParser.run . (flip Input) 0
    initial_part = "global _start\nextern printi\n\nsection .text\n\n"
    final_part = "_start:\n\tpush rbp\n\tmov rbp, rsp\n\n\tcall main\n\n\tpop rbp\n\n\tmov rax, 60\n\tmov rdi, 0\n\tsyscall\n"  

-- if you want a nice repl experience use the readline library wrapper https://github.com/hanslub42/rlwrap
-- repl :: IO()
-- repl = do
--   putStr "haddr >>> "
--   hFlush stdout
--   eof <- hIsEOF stdin
--   if eof then putStrLn "\nexit"
--     else do
--       line <- getLine
--       case line of
--         "exit" -> return ()
--         "" -> repl
--         _ -> do
--           interpret_text line
--           repl
-- 
-- processArgs :: [String] -> IO ()
-- processArgs args
--   | length args == 0 = putStrLn "need at least one subcommand out of repl or interpreter"
--   | args !! 0 == "repl" = repl
--   | args !! 0 == "run" = do
--       fileHandle <- openFile (args !! 1) ReadMode
--       contents <- hGetContents fileHandle
--       interpret_text contents
--   | True = error "invalid subcommand"

main :: IO()
main = do
  args <- getArgs
  return ()
  -- processArgs args
