module Compiler where

import qualified Data.Map as Map
import Text.Printf

import Data.Bool

import Parser

import Control.Applicative
import Data.Foldable

import qualified Debug.Trace as Trace

data Operand = Register String | Addr String | ProcName String | Literal Int deriving (Show, Eq)

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
  deriving (Eq)

data CompilerState = CompilerState {
    counter :: Int,
    symtab :: Map.Map String Symbol,
    rsp :: Int,
    instructions :: [Instruction],
    bss :: [[String]]
  } deriving (Eq)

initialCompilerState :: CompilerState
initialCompilerState = CompilerState 0 Map.empty 0 [] []
-- type Compiler = Expression -> State CompilerState [Instruction]
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

sizeof :: Type -> Int
sizeof (Type_Int) = 8
sizeof (Type_Pointer _) = 8
sizeof (Type_Arr subtype len) = len * (sizeof subtype)

compile_lvalue :: Expression -> Compiler Type
compile_lvalue (Exp_String varname) = do
  state <- get_state
  let var = Map.lookup varname state.symtab
  case var of
    (Just (Sym_Variable pos typename)) -> do
      put_instrs $
        [ Mov (Register "rax") (Register "rbp"),
          Sub (Register "rax") (Literal $ pos)
        ]
      return $ typename
    (Nothing) -> error $ "variable " ++ varname ++ " does not exist"
compile_lvalue (Exp_ArrIndex lvalue index) = do
  lvalue_type <- compile_lvalue lvalue
  let (Type_Arr subtype sublen) = lvalue_type
  put_instr $ Push $ Register "rax"
  index_val <- compile index
  put_instrs $
    [ Mov (Register "rbx") (Literal $ sizeof subtype `div` 8),
      Mul (Register "rbx"),
      Mov (Register "rbx") (Register "rax"),
      Pop $ Register "rax",
      Lea (Register "rax") (Addr $ ("rax [8*rbx]"))
    ]
  return $ subtype
compile_lvalue e = error $ "expression \n" ++ (print_exp 0 e) ++ "is not allowed in lhs right now"


compile :: Expression -> Compiler Type
compile (Exp_Int i) = (put_instrs $ [Mov (Register "rax") (Literal i)]) >> (return Type_Empty)
compile (Exp_String varname) = do
  state <- get_state
  let var = Map.lookup varname state.symtab
  case var of
    (Just pos) -> do
      put_instrs $
        [Mov (Register "rax") (Addr $ ("QWORD [rbp-" ++ (show pos.stackPos) ++ "]"))]
      return Type_Empty
    (Nothing) -> error $ "variable " ++ varname ++ " does not exist"
compile arrindex@(Exp_ArrIndex arr index) = do
  lvalue_type <- compile_lvalue arrindex
  -- put_instr $ Push $ Register "rax"
  -- index_val <- compile index
  put_instrs $
    [ -- Mov (Register "rax") (Register "rbx"),
      -- Pop $ Register "rax",
      -- Add (Register "rax") (Register "rbx"),
      Mov (Register "rax") (Addr "QWORD [rax]")
    ]
  return Type_Empty
compile (Exp_Plus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      put_instr $ Add (Register "rax") (Literal i)
      return Type_Empty
    _ -> do
      put_instr $ Push (Register "rax")
      b <- compile e2
      put_instrs $
        [Pop (Register "rbx")] <>
        [Add (Register "rax") (Register "rbx")]
      return Type_Int
compile (Exp_Minus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      put_instr $ Sub (Register "rax") (Literal i)
      return Type_Empty
    _ -> do
      put_instr $ Push (Register "rax")
      b <- compile e2
      put_instrs $
        [Mov (Register "rbx") (Register "rax")] <>
        [Pop (Register "rax")] <>
        [Sub (Register "rax") (Register "rbx")]
      return Type_Int
compile (Exp_Mult e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  put_instrs $
    [Pop $ Register "rbx"] <>
    [Mul (Register "rbx")]
  return Type_Empty
compile (Exp_Div e1 e2) = do
  a <- compile e1
  put_instr $ Push (Register "rax")
  b <- compile e2
  put_instrs $
    [Mov (Register "rbx") (Register "rax")] <>
    [Pop $ Register "rax"] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div (Register "rbx")]
  return Type_Empty
compile (Exp_Mod e1 e2) = do
  a <- compile e1
  put_instr $ Push (Register "rax")
  b <- compile e2
  put_instrs $
    [Mov (Register "rbx") (Register "rax")] <>
    [Pop $ Register "rax"] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div (Register "rbx")] <>
    [Mov (Register "rax") (Register "rdx")]
  return Type_Empty

compile (Exp_Assignment left_exp right_exp) = do
  lhs <- compile_lvalue left_exp
  put_instr $ Push $ Register "rax"
  rhs <- compile right_exp
  put_instrs $
    [Mov (Register "rbx") (Register "rax")] <>
    [Pop $ Register "rax"] <>
    [Mov (Addr $ "QWORD [rax]") (Register "rbx")]
  put_instr EmptyLine
  return Type_Empty

compile (Exp_LessThan e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  increment_counter
  counter <- (.counter) <$> get_state
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ Register "rbx",
      Cmp (Register "rbx") (Register "rax"),
      Jlt $ label1,
      Mov (Register "rax") (Literal 0),
      Jmp label2,
      Label label1, 
      Mov (Register "rax") (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_GreaterEqual e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  increment_counter
  counter <- (.counter) <$> get_state
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ Register "rbx",
      Cmp (Register "rax") (Register "rbx"),
      Jle $ label1,
      Mov (Register "rax") (Literal 0),
      Jmp label2,
      Label label1, 
      Mov (Register "rax") (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_LessEqual e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  increment_counter
  counter <- (.counter) <$> get_state
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ Register "rbx",
      Cmp (Register "rbx") (Register "rax"),
      Jle $ label1,
      Mov (Register "rax") (Literal 0),
      Jmp label2,
      Label label1, 
      Mov (Register "rax") (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_GreaterThan e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  increment_counter
  counter <- (.counter) <$> get_state
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ Register "rbx",
      Cmp (Register "rax") (Register "rbx"),
      Jlt $ label1,
      Mov (Register "rax") (Literal 0),
      Jmp label2,
      Label label1, 
      Mov (Register "rax") (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_Equality e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  increment_counter
  counter <- (.counter) <$> get_state
  let label1 = ("COMPARISON" ++ (show counter))
  let label2 = ("COMPARISON_END" ++ (show counter))
  put_instrs $ [
      Pop $ Register "rbx",
      Cmp (Register "rax") (Register "rbx"),
      Je $ label1,
      Mov (Register "rax") (Literal 0),
      Jmp label2,
      Label label1, 
      Mov (Register "rax") (Literal 1),
      Label label2
    ]
  return Type_Bool
compile (Exp_And e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  put_instrs $
    [Pop $ Register "rbx"] <>
    [And (Register "rax") (Register "rbx")]
  return Type_Bool
compile (Exp_Or e1 e2) = do
  a <- compile e1
  put_instr $ Push $ Register "rax"
  b <- compile e2
  put_instrs $
    [Pop $ Register "rbx"] <>
    [Or (Register "rax") (Register "rbx")]
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
  put_instrs $ [Cmp (Register "rax") (Literal 0), Je label1]
  true_instrs <- compile true_exp
  put_instrs $ [Jmp label2, Label label1]
  case false_exp of
    Exp_Empty -> return Type_Empty
    _ -> compile false_exp
  put_instr $ Label label2
  return Type_Empty

compile (Exp_While cond_exp body_exp) = do
  increment_counter
  counter <- (.counter) <$> get_state
  let label1 = ("LOOP_START" ++ (show counter))
  let label2 = ("LOOP_END" ++ (show counter))
  put_instr $ Label label1
  cond_type <- compile cond_exp
  case cond_type of
    Type_Bool -> return ()
    _ -> error "condition expression has to be boolean"
  put_instrs $ [Cmp (Register "rax") (Literal 0), Je label2]
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
        (Type_Int) -> do
          state <- get_state
          let pos = state.rsp + 8
          put_state $ state { symtab = Map.insert varname (Sym_Variable pos typename) state.symtab, rsp = pos }
          compile rhs_exp
          put_instrs $ [ Push $ Register "rax" ]
        (Type_Arr subtype length) -> do
          state <- get_state
          let subsize = sizeof subtype
          let arr_size = subsize * length
          put_state $ state { rsp = state.rsp + arr_size, symtab = Map.insert varname (Sym_Variable (state.rsp + arr_size) typename) state.symtab }
          put_instrs $ [ Sub (Register "rsp") (Literal $ length * subsize) ]
      return Type_Empty

compile (Exp_Proc (Exp_String name) args body) = do
  put_instrs $ [ Label name, Push $ Register "rbp", Mov (Register "rbp") (Register "rsp") ]
  old_state <- get_state
  push_args_instrs <- push_args args args_registers
  body_instrs <- compile body
  new_state <- get_state
  put_state $ new_state { rsp = old_state.rsp, symtab = old_state.symtab }
  put_instrs $ [ Add (Register "rsp") (Literal $ 8 * (length args)) ] <> [ Pop (Register "rbp"), Ret ]
  return Type_Empty
    where
      args_registers = take (length args) $ Register <$> ["rdi", "rsi", "rcx", "rdx", "r8", "r9"]
      push_args :: [Expression] -> [Operand] -> Compiler Type
      push_args [] [] = return Type_Empty
      push_args ((Exp_Declaration arg_name typename _):args) (reg:regs) = do
        let size = sizeof typename
        modify_state $ \st -> st { rsp = st.rsp + size, symtab = Map.insert arg_name (Sym_Variable (st.rsp + size) typename) st.symtab }
        put_instr $ Push reg
        rest <- push_args args regs
        return Type_Empty

      -- push_args ((Exp_String arg_name):args) (reg:regs) = do
      --   modify_state $ \st -> st { rsp = st.rsp + 8, symtab = Map.insert arg_name (Sym_Variable (st.rsp + 8) Type_Int) st.symtab }
      --   put_instr $ Push reg
      --   rest <- push_args args regs
      --   return Type_Empty

compile (Exp_ProcCall name' args') = do
  let name = (\e -> case e of
                         Exp_String s -> s
                         _ -> error $ "called name must be a string(for now)" ++ (show args')) name'
  process_args $ args'
  
  put_instrs $ (Pop <$> reverse args_registers) 
  put_instr $ Call $ ProcName name
  return Type_Empty
    where
      args_registers = take (length args') $ Register <$> ["rdi", "rsi", "rcx", "rdx"]
      process_args :: [Expression] -> Compiler ()
      process_args [] = return ()
      process_args (exp:exprs) = do
        compile exp
        modify_state $ (\st -> st { rsp = st.rsp + 8 })
        put_instr $ Push $ Register "rax"
        process_args exprs
        modify_state $ (\st -> st { rsp = st.rsp - 8 })

compile (Exp_SourceBlock exprs) = do
  old_state <- get_state
  compile_exprs exprs
  new_state <- get_state
  modify_state $ \st -> st { rsp = old_state.rsp, symtab = old_state.symtab }
  put_instrs $ [ Add (Register "rsp") (Literal $ 0 - old_state.rsp + new_state.rsp) ]
  return Type_Empty
    where
      compile_exprs :: [Expression] -> Compiler ()
      compile_exprs (exp:exprs) = do
        compile exp
        compile_exprs exprs
      compile_exprs [] = return ()

compile (Exp_Empty) = return Type_Empty

compile e = error $ "unhandled expression: \n" ++ (print_exp 0 e)

sourceCompiler :: [Expression] -> [Instruction]
sourceCompiler exprs = (fst $ compiled.run initialCompilerState).instructions
  where
    compiled = compile_all exprs
    compile_all :: [Expression] -> Compiler ()
    compile_all (e:es) = do
      compile e
      compile_all es
    compile_all [] = return ()
