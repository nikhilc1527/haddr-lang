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

dumpASM :: String -> IO()
dumpASM str = do
  let is = instrs str
  let asm = initial_part ++ is ++ final_part
  putStr asm
  where
    instrs = printInstrs . ((flip evalState) $ CompilerState 0 Map.empty 0) . compile . either (const Exp_Empty) (fst) . (parseBlock :: Parser Char Expression).run . (flip Input) 0
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
  hClose handle
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
    instrs = printInstrs . ((flip evalState) $ CompilerState 0 Map.empty 0) . compile . either (const Exp_Empty) (fst) . (parseBlock :: Parser Char Expression).run . (flip Input) 0
    initial_part = "global main\nextern printi\n\nsection .text\n\nmain:\n\tpush rbp\n\tmov rbp, rsp\n\n"
    final_part = "\n\tmov rdi, rax\n\tcall printi\n\n\tpop rbp\n\n\tmov rax, 0\n\tret\n"

parseAndCompileFile :: FilePath -> IO ()
parseAndCompileFile filepath = do
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  let parsed = sourceFileParser.run $ Input contents 0
  case parsed of
    Left err -> do
      putStrLn $ show err
    Right (parsed_exp, _) -> do
      let instrs = evalState (compile $ Exp_SourceBlock parsed_exp) initialCompilerState
      let instrs_showed = printInstrs instrs
      putStrLn $ instrs_showed

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
