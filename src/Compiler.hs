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

import Data.Bool

import Parser

import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.Foldable

import qualified Debug.Trace as Trace

data Operand = Register String | Addr String | ProcName String | Literal Int deriving (Show)
-- reg = Register

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
  Syscall |
  Ret
  deriving (Show)

data CompilerState =
  CompilerState
  {
    counter :: Int,
    symtab :: Map.Map String Int,
    rsp :: Int
  }
initialCompilerState :: CompilerState
initialCompilerState = CompilerState 0 Map.empty 0
type Compiler = Expression -> State CompilerState [Instruction]

printOperand :: Operand -> String
printOperand (Register s) = s
printOperand (Addr s) = s
printOperand (Literal i) = show i
printOperand (ProcName s) = s

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
printInstrs ((Ret):rest) = "\tret\n" ++ (printInstrs rest)
printInstrs ((Call e1):rest) = "\tcall " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)

-- movr2 a b = Mov (Register a) (Register b)

incCounter :: CompilerState -> CompilerState
incCounter a = a {counter = a.counter + 1}

compile :: Compiler
compile (Exp_Int i) = return [Mov (Register "rax") (Literal i)]
compile (Exp_String varname) = do
  state <- get
  let var = Map.lookup varname state.symtab
  case var of
    (Just pos) -> do
      return $
        [Mov (Register "rax") (Addr $ ("QWORD [rbp-" ++ (show pos) ++ "]"))]
    (Nothing) -> error $ "variable " ++ varname ++ " does not exist"
compile (Exp_Plus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      return $ a <> [Add (Register "rax") (Literal i)]
    -- (Exp_String s) -> do
    --   return $ a <> [Add (Register "rax") (Addr s)]
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
    -- (Exp_String s) -> do
    --   return $ a <> [Sub (Register "rax") (Variable s)]
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
  modify incCounter
  state <- get
  let label1 = ("ELSE" ++ (show state.counter))
  let label2 = ("END" ++ (show state.counter))
  return $
    cond_instrs <>
    [Cmp (Register "rax") (Literal 0),
     Je label1] <>
    true_instrs <>
    [Jmp label2,
     Label label1] <>
    false_instrs <>
    [Label label2]

compile (Exp_While cond_exp body_exp) = do
  cond_instrs <- compile cond_exp
  body_instrs <- compile body_exp
  modify incCounter
  counter <- (.counter) <$> get
  let label1 = ("LOOP_START" ++ (show counter))
  let label2 = ("LOOP_END" ++ (show counter))
  return $
    [Label label1] <>
    cond_instrs <>
    [Cmp (Register "rax") (Literal 0),
     Je label2] <>
    body_instrs <>
    [Jmp label1,
     Label label2]

compile (Exp_Assignment left_exp right_exp) = do
  let varname = case left_exp of
                  (Exp_String s) -> s
                  _ -> error "only have variable names on lhs" -- TODO: allow array indexing, etc on lhs
  state <- get
  let var = Map.lookup varname state.symtab
  case var of
    (Just pos) -> do
      rhs <- compile right_exp
      return $
        rhs <>
        [Mov (Addr $ ("QWORD [rbp-" ++ (show pos) ++ "]")) (Register "rax")]
    (Nothing) -> error "variable doesnt exist"

compile (Exp_Declaration varname typename rhs_exp) = do
  state <- get
  let var = Map.lookup varname state.symtab
  case var of
    (Just pos) -> error "variable exists"
    (Nothing) -> do
      let pos = state.rsp + 8
      put $ state {symtab = Map.insert varname pos state.symtab, rsp = pos}
      rhs <- compile rhs_exp
      return $
        rhs <>
        -- [Mov (Addr $ ("QWORD [rbp-" ++ (show pos) ++ "]")) (Register "rax")] <>
        -- [Sub (Register "rsp") (Literal 8)]
        [ Push $ Register "rax" ]

compile (Exp_Proc name args body) = case name of
  Exp_String name_s -> do
    push_args_instrs <- push_args args args_registers
    body_instrs <- compile body
    return $ [ Label name_s, Push $ Register "rbp", Mov (Register "rbp") (Register "rsp") ] <> push_args_instrs <> body_instrs <> [ Add (Register "rsp") (Literal $ 8 * (length args)) ] <> [ Pop (Register "rbp"), Ret ]
      where
        args_registers = take (length args) $ Register <$> ["rdi", "rsi", "rcx", "rdx"]
        push_args :: [Expression] -> [Operand] -> State CompilerState [Instruction]
        push_args [] [] = return [] 
        push_args (arg:args) (reg:regs) = case arg of
          (Exp_String arg_name) -> do
            modify $ \st -> st { rsp = st.rsp + 8, symtab = Map.insert arg_name (st.rsp + 8) st.symtab }
            rest <- push_args args regs
            return $ [ Push reg ] <> rest
          _ -> error "name of procedure argument has to be a string"
  _ -> error "name of proc has to be string"

compile (Exp_ProcCall name' args') = do
  let name = (\e -> case e of
                         Exp_String s -> s
                         _ -> error $ "called name must be a string(for now)" ++ (show args')) name'
  args_instrs <- process_args $ args'
  -- modify $ (\st -> st { rsp = st.rsp + 8 * (length args') })
  
  return $ args_instrs <> (Pop <$> reverse args_registers) <> [Call $ ProcName name]
    where
      args_registers = take (length args') $ Register <$> ["rdi", "rsi", "rcx", "rdx"]

      process_args :: [Expression] -> State CompilerState [Instruction]
      process_args [] = return []
      process_args (exp:exprs) = do
        instrs <- compile exp
        modify $ (\st -> st { rsp = st.rsp + 8 })
        rest <- process_args exprs
        modify $ (\st -> st { rsp = st.rsp - 8 })
        return $ instrs <> [Push $ Register "rax"] <> rest

-- compile (Exp_SourceBlock []) = do
--   state <- get
--   return $ [Add (Register "rsp") (Literal state.rsp)]
-- compile (Exp_SourceBlock (exp:exprs)) = do
--   a <- compile exp
--   b <- compile $ Exp_SourceBlock exprs
--   return $ a ++ b
compile (Exp_SourceBlock exprs) = do
  old_state <- get
  instrs <- get_instrs exprs
  new_state <- get
  modify $ (\st -> st { rsp = old_state.rsp, symtab = old_state.symtab })
  return $ instrs <> [ Add (Register "rsp") (Literal $ new_state.rsp - old_state.rsp) ]
    where
      get_instrs :: [Expression] -> State CompilerState [Instruction]
      get_instrs (exp:exprs) = do
        a <- compile exp
        b <- get_instrs exprs
        return $ a <> b
      get_instrs [] = do
        return []

compile e = error $ "unhandled expression: \n" ++ (print_exp 0 e)
