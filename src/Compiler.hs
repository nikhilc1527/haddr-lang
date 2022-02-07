module Compiler where

import qualified Data.Map as Map
import Text.Printf

import Data.Bool
import qualified Data.Set as Set

import Parser

import Control.Applicative
import Data.Foldable

import qualified Debug.Trace as Trace

register_list :: [String]
register_list = ["rdi", "rsi", "rcx", "rdx", "r8", "r9"]

data Operand = Register String | Addr String | ProcName String | Literal Int deriving (Show, Eq)

rax = Register "rax"
rbx = Register "rbx"
al = Register "al"
bl = Register "bl"
rbp = Register "rbp"
rsp = Register "rsp"

data Instruction =
  Comment String |
  EmptyLine |
  Lea Operand Operand |
  Mov Operand Operand |
  Add Operand Operand |
  Or Operand Operand |
  And Operand Operand |
  Call Operand |
  Cmp Operand Operand |
  Je String |
  Jmp String |
  Jlt String |
  Jle String |
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
  deriving (Show, Eq)

data Symbol =
  Sym_Variable { stackPos :: Int, typename :: Type } |
  Sym_Function { name :: String, params :: [Type], res :: Type }
  deriving (Show, Eq)

data CompilerState = CompilerState {
    counter :: Int,
    symtab :: Map.Map String Symbol,
    rsp :: Int,
    instructions :: [Instruction],
    bss :: [(String, String)],
    cur_block :: Int,
    procs :: Set.Set String
  } deriving (Eq)

initialSymtab :: Map.Map String Symbol
initialSymtab = Map.fromList 
  [
    ("getch", Sym_Function "getch" [] Type_I64),
    ("putch", Sym_Function "putch" [Type_I64] Type_Empty),
    ("unbuffer_term", Sym_Function "unbuffer_term" [] Type_Empty),
    ("nonblock", Sym_Function "nonblock" [] Type_Empty),
    ("flush_out", Sym_Function "flush_out" [] Type_Empty),
    ("puti", Sym_Function "puti" [Type_I64] Type_Empty),
    ("printi", Sym_Function "printi" [Type_I64] Type_Empty),
    ("puts", Sym_Function "puts" [Type_Pointer Type_I64] Type_Empty),
    ("sleep_for", Sym_Function "sleep_for" [Type_I64] Type_Empty),
    ("free", Sym_Function "free" [Type_Pointer Type_I64] Type_Empty),
    ("malloc", Sym_Function "malloc" [] (Type_Pointer Type_I64))
  ]

initialCompilerState :: CompilerState
initialCompilerState = CompilerState 0 initialSymtab 0 [] [] 0 (Set.fromList ["flush_out"])

newtype Compiler a = Compiler { run :: (CompilerState) -> (CompilerState, a) }

instance Functor Compiler where
  fmap f (Compiler comp) = Compiler $ \input -> let (state, res) = comp input in (state, f res)

instance Applicative Compiler where
  pure a = Compiler $ \is -> (is, a)
  (Compiler a) <*> (Compiler b) = Compiler $ \input -> 
    let
      (s1, f) = a input
      (s2, x) = b s1
    in
      (s2, f x)

instance Monad Compiler where
  return = pure

  Compiler comp >>= k = Compiler $ \input ->
    let
      (state, result) = comp input
    in 
      (k result).run state

get_state :: Compiler CompilerState
get_state = Compiler $ \state -> (state, state)

get_counter :: Compiler Int
get_counter = Compiler $ \state -> (state, state.counter)

put_state :: CompilerState -> Compiler ()
put_state new_state = Compiler $ const (new_state, ())

modify_state :: (CompilerState -> CompilerState) -> Compiler ()
modify_state f = Compiler $ \state -> (f state, ())

put_instr :: Instruction -> Compiler ()
put_instr instr = put_instrs $ [instr]

put_instrs :: [Instruction] -> Compiler ()
put_instrs instrs = Compiler $ \state -> (state { instructions = state.instructions ++ instrs }, ())

modify_symtab :: (Map.Map String Symbol -> Map.Map String Symbol) -> Compiler ()
modify_symtab mod = Compiler $ \state -> (state { symtab = mod $ state.symtab }, () )

modify_rsp :: (Int -> Int) -> Compiler ()
modify_rsp mod = Compiler $ \state -> (state { rsp = mod $ state.rsp }, () )

increment_counter :: Compiler ()
increment_counter = Compiler $ \state -> (state { counter = state.counter + 1 }, () )

get_block :: Compiler Int
get_block = Compiler $ \state -> (state, state.cur_block)

put_block :: Int -> Compiler ()
put_block block = Compiler $ \state -> (state { cur_block = block }, ())

printOperand :: Operand -> String
printOperand (Register s) = s
printOperand (Addr s) = s
printOperand (Literal i) = show i
printOperand (ProcName s) = s

printInstrs :: [Instruction] -> String
printInstrs [] = ""
printInstrs ((Comment s):rest) = "\t;; " ++ s ++ "\n" ++ (printInstrs rest)
printInstrs (EmptyLine:rest) = "\n" ++ (printInstrs rest)
printInstrs ((Mov e1 e2):rest) = "\tmov " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Lea e1 e2):rest) = "\tlea " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Add e1 e2):rest) = "\tadd " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Sub e1 e2):rest) = "\tsub " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Cmp e1 e2):rest) = "\tcmp " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((And e1 e2):rest) = "\tand " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Or e1 e2):rest) = "\tor " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Mul e1):rest) = "\tmul " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Div e1):rest) = "\tdiv " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Push e1):rest) = "\tpush " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Pop e1):rest) = "\tpop " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Je e1):rest) = "\tje " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jmp e1):rest) = "\tjmp " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jlt e1):rest) = "\tjl " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jle e1):rest) = "\tjle " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Label s):rest) = s ++ ":\n" ++ (printInstrs rest)
printInstrs ((Ret):rest) = "\tret\n" ++ (printInstrs rest)
printInstrs ((Call e1):rest) = "\tcall " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)

compile_lvalue :: Expression -> Compiler Type
compile_lvalue (Exp_String varname) = do
  state <- get_state
  let var = Map.lookup varname state.symtab
  case var of
    (Just (Sym_Variable pos typename)) -> do
      put_instrs $
        [ Mov rax rbp,
          Sub rax (Literal $ pos)
        ]
      return $ typename
    (Nothing) -> error $ "variable " ++ varname ++ " does not exist"
compile_lvalue (Exp_ArrIndex lvalue index) = do
  lvalue_type <- compile_lvalue lvalue
  case lvalue_type of
    Type_Arr subtype sublen -> do
      put_instr $ Push $ rax
      index_val <- compile index
      put_instrs $
        [ Mov rbx (Literal $ sizeof subtype),
          Mul rbx,
          Mov rbx rax,
          Pop $ rax,
          Lea rax (Addr $ ("rax [rbx]"))
        ]
      return $ subtype
    Type_Pointer subtype -> do
      put_instr $ Mov rax (Addr "QWORD [rax]")
      put_instr $ Push $ rax
      index_val <- compile index
      put_instrs $
        [ Mov rbx (Literal $ sizeof subtype),
          Mul rbx,
          Mov rbx rax,
          Pop rax,
          Lea rax (Addr $ ("rax [rbx]"))
        ]
      return $ subtype
compile_lvalue e = error $ "expression \n" ++ (print_exp 0 e) ++ "is not allowed in lhs right now"

compile :: Expression -> Compiler Type
compile (Exp_Int i) = (put_instrs $ [Mov rax (Literal i)]) >> (return Type_Empty)
compile (Exp_StringLiteral s) = do
  counter <- get_counter
  increment_counter
  let bssname = "BSS" ++ (show counter)
  Compiler $ \state -> (state { bss = state.bss ++ [(s, bssname)] }, ())
  put_instr $ Mov rax (Addr bssname)
  return $ Type_Pointer Type_I8
compile (Exp_String varname) = do
  state <- get_state
  let var = Map.lookup varname state.symtab
  case var of
    (Just (Sym_Variable pos Type_I64)) -> do
      put_instrs $
        [Mov rax (Addr $ ("QWORD [rbp-" ++ (show pos) ++ "]"))]
      return Type_I64
    (Just (Sym_Variable pos Type_I8)) -> do
      put_instrs $
        [Mov rbx $ Literal 0, Mov bl (Addr $ ("BYTE [rbp-" ++ (show pos) ++ "]")), Mov rax rbx]
      return Type_I64
    (Just (Sym_Variable pos typename@(Type_Pointer _))) -> do
      put_instrs $
        [Mov rax (Addr $ ("QWORD [rbp-" ++ (show pos) ++ "]"))]
      return typename
    (Just (Sym_Variable pos typename@(Type_Arr _ _))) -> do
      put_instrs $
        [Lea rax (Addr $ ("QWORD [rbp-" ++ (show pos) ++ "]"))]
      return typename
    (Just (Sym_Function name params res)) -> do
      put_instr $ Mov rax (Addr name)
      Compiler $ \state -> (state { procs = Set.insert name state.procs }, ())
      return $ Type_Func params res
    (Just bla) -> error $ "unhandled: " ++ (show bla)
    (Nothing) -> error $ "variable " ++ varname ++ " does not exist"
compile arrindex@(Exp_ArrIndex arr index) = do
  lvalue_type <- compile_lvalue arrindex
  case lvalue_type of
    Type_I64 -> put_instr $ Mov rax (Addr "QWORD [rax]")
    Type_I8 -> do
      put_instr $ Mov rbx $ Literal 0
      put_instr $ Mov bl (Addr "BYTE [rax]")
      put_instr $ Mov rax rbx
  return Type_Empty
compile (Exp_Plus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      put_instr $ Add rax (Literal i)
      return Type_Empty
    _ -> do
      put_instr $ Push rax
      b <- compile e2
      put_instrs $
        [Pop rbx] <>
        [Add rax rbx]
      return Type_I64
compile (Exp_Minus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      put_instr $ Sub rax (Literal i)
      return Type_Empty
    _ -> do
      put_instr $ Push rax
      b <- compile e2
      put_instrs $
        [Mov rbx rax] <>
        [Pop rax] <>
        [Sub rax rbx]
      return Type_I64
compile (Exp_Mult e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  put_instrs $
    [Pop $ rbx] <>
    [Mul rbx]
  return Type_Empty
compile (Exp_Div e1 e2) = do
  a <- compile e1
  put_instr $ Push rax
  b <- compile e2
  put_instrs $
    [Mov rbx rax] <>
    [Pop $ rax] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div rbx]
  return Type_Empty
compile (Exp_Mod e1 e2) = do
  a <- compile e1
  put_instr $ Push rax
  b <- compile e2
  put_instrs $
    [Mov rbx rax] <>
    [Pop $ rax] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div rbx] <>
    [Mov rax (Register "rdx")]
  return Type_Empty

compile (Exp_Assignment left_exp right_exp) = do
  lhs <- compile_lvalue left_exp
  put_instr $ Push $ rax
  rhs <- compile right_exp
  put_instr $ Mov rbx rax
  put_instr $ Pop $ rax
  case sizeof lhs of
    1 -> put_instr $ Mov (Addr $ "BYTE [rax]") (Register "bl")
    8 -> put_instr $ Mov (Addr $ "QWORD [rax]") rbx
  put_instr EmptyLine
  return Type_Empty

compile (Exp_LessThan e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  increment_counter
  counter <- get_counter
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ rbx,
      Cmp rbx rax,
      Jlt $ label1,
      Mov rax (Literal 0),
      Jmp label2,
      Label label1, 
      Mov rax (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_GreaterEqual e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  increment_counter
  counter <- get_counter
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ rbx,
      Cmp rax rbx,
      Jle $ label1,
      Mov rax (Literal 0),
      Jmp label2,
      Label label1, 
      Mov rax (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_LessEqual e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  increment_counter
  counter <- get_counter
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ rbx,
      Cmp rbx rax,
      Jle $ label1,
      Mov rax (Literal 0),
      Jmp label2,
      Label label1, 
      Mov rax (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_GreaterThan e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  increment_counter
  counter <- get_counter
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ rbx,
      Cmp rax rbx,
      Jlt $ label1,
      Mov rax (Literal 0),
      Jmp label2,
      Label label1, 
      Mov rax (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_Equality e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  increment_counter
  counter <- get_counter
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ rbx,
      Cmp rax rbx,
      Je $ label1,
      Mov rax (Literal 0),
      Jmp label2,
      Label label1, 
      Mov rax (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_And e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  put_instrs $
    [Pop $ rbx] <>
    [And rax rbx]
  return Type_Bool
compile (Exp_Or e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  put_instrs $
    [Pop $ rbx] <>
    [Or rax rbx]
  return Type_Bool

compile (Exp_If cond_exp true_exp false_exp) = do
  increment_counter
  state <- get_state
  let label1 = ("ELSE" ++ (show state.counter))
  let label2 = ("END" ++ (show state.counter))
  cond_type <- compile cond_exp
  case cond_type of
    Type_Bool -> return ()
    _ -> error "condition expression has to be boolean"
  put_instrs $ [Cmp rax (Literal 0), Je label1]
  true_instrs <- compile true_exp
  put_instrs $ [Jmp label2, Label label1]
  case false_exp of
    Exp_Empty -> return Type_Empty
    _ -> compile false_exp
  put_instr $ Label label2
  return Type_Empty

compile (Exp_While cond_exp body_exp) = do
  increment_counter
  counter <- get_counter
  let label1 = ("LOOP_START" ++ (show counter))
  let label2 = ("LOOP_END" ++ (show counter))
  put_instr $ Label label1
  cond_type <- compile cond_exp
  case cond_type of
    Type_Bool -> return ()
    _ -> error "condition expression has to be boolean"
  put_instrs $ [Cmp rax (Literal 0), Je label2]
  body_instrs <- compile body_exp
  put_instrs $ [Jmp label1, Label label2]
  return Type_Empty

compile (Exp_Declaration varname typename rhs_exp) = do
  symtab <- (.symtab) <$> get_state
  let var = Map.lookup varname symtab
  case var of
    (Just pos) -> error $ "variable exists: " ++ varname
    (Nothing) -> do
      case typename of
        (Type_Pointer subtype) -> do
          state <- get_state
          let pos = state.rsp + 8
          put_state $ state { symtab = Map.insert varname (Sym_Variable pos typename) state.symtab, rsp = pos }
          compile rhs_exp
          put_instrs $ [ Push $ rax ]
        (Type_I64) -> do
          state <- get_state
          let pos = state.rsp + 8
          put_state $ state { symtab = Map.insert varname (Sym_Variable pos typename) state.symtab, rsp = pos }
          compile rhs_exp
          put_instrs $ [ Push $ rax ]
        (Type_I8) -> do
          state <- get_state
          let pos = state.rsp + 1
          put_state $ state { symtab = Map.insert varname (Sym_Variable pos typename) state.symtab, rsp = pos }
          compile rhs_exp
          put_instrs $ [ Sub rsp $ Literal 1, Mov (Addr $ "BYTE [rsp]") al ]
        (Type_Arr subtype length) -> do
          state <- get_state
          let subsize = sizeof subtype
          let arr_size = subsize * length
          put_state $ state { rsp = state.rsp + arr_size, symtab = Map.insert varname (Sym_Variable (state.rsp + arr_size) typename) state.symtab }
          put_instrs $ [ Sub rsp (Literal $ length * subsize) ]
      return Type_Empty

compile (Exp_Proc (Exp_String name) args body) = do
  let functype = Sym_Function name (map (\(Exp_Declaration _ typename _) -> typename) args) Type_Empty
  modify_symtab $ Map.insert name functype
  put_instrs $ [ Label name, Push $ rbp, Mov rbp rsp ]
  old_state <- get_state
  push_args_instrs <- push_args args args_registers
  body_instrs <- compile body
  new_state <- get_state
  put_state $ new_state { rsp = old_state.rsp, symtab = old_state.symtab }
  put_instrs $ [ Add rsp (Literal $ 8 * (length args)) ] <> [ Pop rbp, Ret ]
  return Type_Empty
    where
      args_registers = take (length args) $ Register <$> register_list
      push_args :: [Expression] -> [Operand] -> Compiler Type
      push_args [] [] = return Type_Empty
      push_args ((Exp_Declaration arg_name typename _):args) (reg:regs) = do
        let size = sizeof typename
        modify_state $ \st -> st { rsp = st.rsp + size, symtab = Map.insert arg_name (Sym_Variable (st.rsp + size) typename) st.symtab }
        put_instr $ Push reg
        rest <- push_args args regs
        return Type_Empty

compile (Exp_ProcCall procname args) = do
  functype <- compile procname
  let (Type_Func params_type res_type) = functype
  put_instr $ Push $ rax
  process_args $ args
  
  put_instrs $ (Pop <$> reverse args_registers) 
  put_instr $ Pop $ rax
  put_instr $ Call $ rax
  return Type_Empty
    where
      args_registers = take (length args) $ Register <$> register_list
      process_args :: [Expression] -> Compiler ()
      process_args [] = return ()
      process_args (exp:exprs) = do
        compile exp
        modify_state $ (\st -> st { rsp = st.rsp + 8 })
        put_instr $ Push $ rax
        process_args exprs
        modify_state $ (\st -> st { rsp = st.rsp - 8 })

compile (Exp_Return exp) = do
  block <- get_block
  compile exp
  put_instr $ Jmp $ "BLOCKEND" ++ (show block)
  return Type_Empty

compile (Exp_SourceBlock exprs) = do
  old_state <- get_state
  increment_counter
  block <- get_counter
  old_block <- get_block
  put_block block

  compile_exprs exprs
  
  new_state <- get_state
  modify_state $ \st -> st { rsp = old_state.rsp, symtab = old_state.symtab }
  put_block old_block
  put_instrs $ [ Label $ "BLOCKEND" ++ (show block), Add rsp (Literal $ new_state.rsp - old_state.rsp) ]
  return Type_Empty
    where
      compile_exprs :: [Expression] -> Compiler ()
      compile_exprs = foldr ((>>) . (>> put_instr EmptyLine) . compile) (pure ())

compile (Exp_Empty) = return Type_Empty

compile e = error $ "unhandled expression in compiler: \n" ++ (print_exp 0 e)

sourceCompiler :: [Expression] -> ([Instruction], [(String, String)], [String])
sourceCompiler exprs = (compile_final_state.instructions, compile_final_state.bss, Set.toList $ compile_final_state.procs)
  where
    compiled = compile_all exprs
    compile_all :: [Expression] -> Compiler ()
    compile_all (e:es) = do
      compile e
      compile_all es
    compile_all [] = return ()
    
    compile_final_state = fst $ compiled.run initialCompilerState
