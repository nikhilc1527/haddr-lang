-- func call:
-- push rbp
-- mov rbp, rsp
-- push arg1
-- push arg2
-- push arg3
-- call func
-- (inside func)
-- arg1 at [rbp-8]
-- arg2 at [rbp-16]
-- arg3 at [rbp-24]
-- var1 at [rbp-40]
-- var2 at [rbp-48]
-- var3 at [rbp-56]
-- pop
-- pop
-- pop
-- ret
-- pop
-- pop
-- pop

module Compiler where

import qualified Data.Map as Map
import Text.Printf

import Parser2

import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.Foldable
import System.IO
import System.Process

import qualified Debug.Trace as Trace

data Operand = Register String | Variable String | Literal Int deriving (Show)
reg = Register

data Instruction =
  Mov Operand Operand |
  Add Operand Operand |
  Call Operand |
  Cmp Operand Operand |
  Je String |
  Jmp String |
  Div Operand |
  Mod Operand Operand |
  Interrupt Int |
  Mul Operand |
  Pop Operand |
  Push Operand |
  Return |
  Sub Operand Operand |
  Label String |
  Syscall
  deriving (Show)

type Compiler = Expression -> State (Int, Map.Map String Int) [Instruction]

printOperand :: Operand -> String
printOperand (Register s) = s
printOperand (Variable i) = ""
printOperand (Literal i) = show i

printInstrs :: [Instruction] -> String
printInstrs [] = ""
printInstrs ((Mov e1 e2):rest) = "\tmov " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Add e1 e2):rest) = "\tadd " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Sub e1 e2):rest) = "\tsub " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Cmp e1 e2):rest) = "\tcmp " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Mul e1):rest) = "\tmul " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Div e1):rest) = "\tdiv " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Push e1):rest) = "\tpush " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Pop e1):rest) = "\tpop " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Je e1):rest) = "\tje " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jmp e1):rest) = "\tjmp " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Label s):rest) = s ++ ":\n" ++ (printInstrs rest)

-- movr2 a b = Mov (Register a) (Register b)

compile :: Compiler
compile (Exp_Int i) = return [Mov (Register "rax") (Literal i)]
compile (Exp_Plus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      return $ a <> [Add (Register "rax") (Literal i)]
    (Exp_String s) -> do
      return $ a <> [Add (Register "rax") (Variable s)]
    _ -> do
      b <- compile e2
      return $
        a <>
        [Push (Register "rax")] <>
        b <>
        [Pop (Register "rbx")] <>
        [Add (Register "rax") (Register "rbx")]
compile (Exp_Minus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      return $ a <> [Sub (Register "rax") (Literal i)]
    (Exp_String s) -> do
      return $ a <> [Sub (Register "rax") (Variable s)]
    _ -> do
      b <- compile e2
      return $
        a <>
        [Push (Register "rax")] <>
        b <>
        [Mov (Register "rbx") (Register "rax")] <>
        [Pop $ Register "rax"] <>
        [Sub (Register "rax") (Register "rbx")]
compile (Exp_Mult e1 e2) = do
  a <- compile e1
  b <- compile e2
  return $
    a <>
    [Push (Register "rax")] <>
    b <>
    [Pop $ Register "rbx"] <>
    [Mul (Register "rbx")]
compile (Exp_Div e1 e2) = do
  a <- compile e1
  b <- compile e2
  return $
    a <>
    [Push (Register "rax")] <>
    b <>
    [Mov (Register "rbx") (Register "rax")] <>
    [Pop $ Register "rax"] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div (Register "rbx")]
compile (Exp_Mod e1 e2) = do
  a <- compile e1
  b <- compile e2
  return $
    a <>
    [Push (Register "rax")] <>
    b <>
    [Mov (Register "rbx") (Register "rax")] <>
    [Pop $ Register "rax"] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div (Register "rbx")] <>
    [Mov (Register "rax") (Register "rdx")]

{-
cond_instrs
cmp rax, 0
je else_branch
true_instrs
jmp end
else_branch:
false_instrs
end
-}
compile (Exp_If cond_exp true_exp false_exp) = do
  cond_instrs <- compile cond_exp
  true_instrs <- compile true_exp
  false_instrs <- compile false_exp
  modify incIfCounter
  (counter, _) <- get
  return $
    cond_instrs <>
    [Cmp (Register "rax") (Literal 0),
     Je $ ("ELSE" ++ (show counter))] <>
    true_instrs <>
    [Jmp $ ("END" ++ (show counter)),
     Label $ ("ELSE" ++ (show counter))] <>
    false_instrs <>
    [Label $ ("END" ++ (show counter))]
    where
      incIfCounter :: (Int, Map.Map String Int) -> (Int, Map.Map String Int)
      incIfCounter (a, b) = (a+1, b)

compile (Exp_SourceBlock []) = return []
compile (Exp_SourceBlock (exp:exprs)) = do
  a <- compile exp
  b <- compile $ Exp_SourceBlock exprs
  return $ a ++ b

parseAndCompileExp :: String -> IO ()
parseAndCompileExp str = do
  let is = instrs str
  handle <- openFile "/mnt/sda2/Nikhil/projects/asm_testing/main.asm" WriteMode
  hPutStr handle initial_part
  hPutStr handle is
  hPutStr handle final_part
  hFlush handle
  let process = (shell "make -B -s && ./main") {cwd = Just "/mnt/sda2/Nikhil/projects/asm_testing/"}
  output <- readCreateProcess process ""
  putStr output
  where
    instrs = printInstrs . ((flip evalState) (0, Map.empty)) . compile . either (const Exp_Empty) (fst) . (statementP :: Parser Char (Error Char String) Expression).run . (flip Input) 0
    initial_part = "global main\nextern printi\n\nsection .text\n\nmain:\n\tpush rbp\n\tmov rbp, rsp\n\n"
    final_part = "\n\tmov rdi, rax\n\tcall printi\n\n\tpop rbp\n\n\tmov rax, 0\n\tret\n"
