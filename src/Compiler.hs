module Compiler where

import qualified Data.Map as Map
import Text.Printf

import Data.Bool
import qualified Data.Set as Set

import Parser

import Control.Applicative
import Control.Exception
import Data.Foldable

import qualified Debug.Trace as Trace

register_list :: [String]
register_list = ["rdi", "rsi", "rcx", "rdx", "r8", "r9", "r11"]

sizeof :: Type -> Compiler Int
sizeof (Type_I64) = return 8
sizeof (Type_I32) = return 4
sizeof (Type_I8) = return 1
sizeof (Type_Pointer _) = return 8
sizeof (Type_Arr subtype len) = do
  subsize <- (sizeof subtype)
  lensize <- eval_constexpr len
  let lensize_int = case lensize of
                      (Exp_Int i) -> i
                      e -> error $ "cannot take size of " ++ (show e)
  return $ lensize_int * subsize
sizeof e = error $ "cannot take size of type " ++ (show e)

data Operand = Register String | Addr String | ProcName String | Literal Int deriving (Show, Eq)

rax = Register "rax"
rbx = Register "rbx"
rcx = Register "rcx"
rdx = Register "rdx"
r8 = Register "r8"
r9 = Register "r9"
r10 = Register "r10"
r11 = Register "r11"
al = Register "al"
bl = Register "bl"
eax = Register "eax"
ebx = Register "ebx"
rbp = Register "rbp"
rsp = Register "rsp"
rdi = Register "rdi"
rsi = Register "rsi"

data Instruction =
  Comment String |
  EmptyLine |
  Lea Operand Operand |
  Mov Operand Operand |
  Add Operand Operand |
  Or Operand Operand |
  And Operand Operand |
  Xor Operand Operand |
  Call Operand |
  Cmp Operand Operand |
  Je String |
  Jne String |
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
    cur_proc :: Int,
    procs :: Set.Set String,
    modul :: Map.Map String Symbol,
    constexprs :: Map.Map String (Type, Expression)
  } deriving (Eq)

initialSymtab :: Map.Map String Symbol
initialSymtab = Map.fromList 
  [
    ("getch", Sym_Function "getch" [] Type_I8),
    ("putch", Sym_Function "putch" [Type_I8] Type_Empty),
    ("unbuffer_term", Sym_Function "unbuffer_term" [] Type_Empty),
    ("nonblock", Sym_Function "nonblock" [] Type_Empty),
    ("flush_out", Sym_Function "flush_out" [] Type_Empty),
    ("puti", Sym_Function "puti" [Type_I64] Type_Empty),
    ("printi", Sym_Function "printi" [Type_I64] Type_Empty),
    ("puts", Sym_Function "puts" [Type_Pointer Type_I8] Type_Empty),
    ("sleep_for", Sym_Function "sleep_for" [Type_I64] Type_Empty),
    ("free", Sym_Function "free" [Type_Pointer Type_I64] Type_Empty),
    ("malloc", Sym_Function "malloc" [Type_I64] (Type_Pointer Type_I64)),
    ("syscall0", Sym_Function "syscall0" [Type_I64] (Type_Empty)),
    ("syscall1", Sym_Function "syscall1" [Type_I64, Type_Any] (Type_Empty)),
    ("syscall2", Sym_Function "syscall2" [Type_I64, Type_Any, Type_Any] (Type_Empty)),
    ("syscall3", Sym_Function "syscall3" [Type_I64, Type_Any, Type_Any, Type_Any] (Type_Empty)),
    ("syscall4", Sym_Function "syscall4" [Type_I64, Type_Any, Type_Any, Type_Any, Type_Any] (Type_Empty)),
    ("syscall5", Sym_Function "syscall5" [Type_I64, Type_Any, Type_Any, Type_Any, Type_Any, Type_Any] (Type_Empty)),
    ("syscall6", Sym_Function "syscall6" [Type_I64, Type_Any, Type_Any, Type_Any, Type_Any, Type_Any, Type_Any] (Type_Empty))
  ]

initialInstrs :: [Instruction]
initialInstrs = concat $ [
    syscall0_instrs,
    syscall1_instrs,
    syscall2_instrs,
    syscall3_instrs,
    syscall4_instrs,
    syscall5_instrs,
    syscall6_instrs
  ]
  where
    -- rdi, rsi, rcx, rdx, r8,  r9, r11
    -- rax, rdi, rsi, rdx, r10, r8, r9
    syscall0_instrs = [Label "syscall0", Mov rax rdi, Syscall, Ret]
    syscall1_instrs = [Label "syscall1", Mov rax rdi, Mov rdi rsi, Syscall, Ret]
    syscall2_instrs = [Label "syscall2", Mov rax rdi, Mov rdi rsi, Mov rsi rcx, Syscall, Ret]
    syscall3_instrs = [Label "syscall3", Mov rax rdi, Mov rdi rsi, Mov rsi rcx, Syscall, Ret]
    syscall4_instrs = [Label "syscall4", Mov rax rdi, Mov rdi rsi, Mov rsi rcx, Mov r10 r8, Syscall, Ret]
    syscall5_instrs = [Label "syscall5", Mov rax rdi, Mov rdi rsi, Mov rsi rcx, Mov r10 r8, Mov r9 r8, Syscall, Ret]
    syscall6_instrs = [Label "syscall6", Mov rax rdi, Mov rdi rsi, Mov rsi rcx, Mov r10 r8, Mov r8 r9, Mov r9 r11, Syscall, Ret]

initialCompilerState :: CompilerState
initialCompilerState = CompilerState 0 initialSymtab 0 initialInstrs [] 0 0 (Set.fromList ["flush_out"]) Map.empty Map.empty

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

get_proc :: Compiler Int
get_proc = Compiler $ \state -> (state, state.cur_proc)

put_proc :: Int -> Compiler ()
put_proc new_state = Compiler $ \st -> (st {cur_proc = new_state}, ())

add_to_modul :: Symbol -> Compiler ()
add_to_modul sym@(Sym_Function name params rettype) = Compiler $ \state -> (state { modul = Map.insert name sym state.modul }, ())

put_contexpr :: String -> Type -> Expression -> Compiler ()
put_contexpr name typename expr = Compiler $ \state -> (state { constexprs = Map.insert name (typename, expr) state.constexprs }, ())

modify_constexpr :: String -> Type -> Expression -> Compiler ()
modify_constexpr name typename expr = Compiler $ \state -> (state { constexprs = Map.insert name (typename, expr) $ Map.delete name state.constexprs }, ())

get_constexpr :: String -> Compiler (Maybe (Type, Expression))
get_constexpr name = Compiler $ \state -> (state, Map.lookup name state.constexprs)

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
printInstrs ((Xor e1 e2):rest) = "\txor " ++ (printOperand e1) ++ ", " ++ (printOperand e2) ++ "\n" ++ (printInstrs rest)
printInstrs ((Mul e1):rest) = "\tmul " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Div e1):rest) = "\tdiv " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Push e1):rest) = "\tpush " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Pop e1):rest) = "\tpop " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)
printInstrs ((Je e1):rest) = "\tje " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jne e1):rest) = "\tjne " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jmp e1):rest) = "\tjmp " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jlt e1):rest) = "\tjl " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Jle e1):rest) = "\tjle " ++ e1 ++ "\n" ++ (printInstrs rest)
printInstrs ((Label s):rest) = s ++ ":\n" ++ (printInstrs rest)
printInstrs ((Ret):rest) = "\tret\n" ++ (printInstrs rest)
printInstrs ((Syscall):rest) = "\tsyscall\n" ++ (printInstrs rest)
printInstrs ((Call e1):rest) = "\tcall " ++ (printOperand e1) ++ "\n" ++ (printInstrs rest)

type_equivalent :: Type -> Type -> Bool
type_equivalent = (==)

type_can_be :: Type -> Type -> Bool
type_can_be (Type_Arr sub1 len) (Type_Pointer sub2) = sub1 `type_can_be` sub2
type_can_be (Type_Pointer _) (Type_I64) = True
type_can_be (Type_I8) (Type_I64) = True
type_can_be (Type_I8) (Type_I32) = True
type_can_be (Type_I32) (Type_I8) = True
type_can_be (Type_I32) (Type_I64) = True
type_can_be (Type_I64) (Type_I8) = True
type_can_be (Type_I64) (Type_I32) = True
type_can_be t1 Type_Any = True
type_can_be t1 t2 = t1 == t2

canonicalize_type :: Type -> Compiler Type
canonicalize_type (Type_Arr subtype len) = do
  subcanon <- canonicalize_type subtype
  lenexp <- eval_constexpr len
  return $ Type_Arr subcanon lenexp
canonicalize_type (Type_Pointer subtype) = (canonicalize_type subtype) >>= (\t -> return $ Type_Pointer t)
canonicalize_type a = return a

eval_constexpr :: Expression -> Compiler Expression
eval_constexpr e@(Exp_Int i) = return e
eval_constexpr (Exp_Plus e1 e2) = do
  exp1 <- eval_constexpr e1
  let exp1_int = case exp1 of
                   (Exp_Int i) -> i
                   e -> error $ "cannot evaluate " ++ (show e) ++ " in a constant expression"
  exp2 <- eval_constexpr e2
  let exp2_int = case exp2 of
                   (Exp_Int i) -> i
                   e -> error $ "cannot evaluate " ++ (show e) ++ " in a constant expression"
  return $ Exp_Int $ exp1_int + exp2_int
eval_constexpr (Exp_Mult e1 e2) = do
  exp1 <- eval_constexpr e1
  let exp1_int = case exp1 of
                   (Exp_Int i) -> i
                   e -> error $ "cannot evaluate " ++ (show e) ++ " in a constant expression"
  exp2 <- eval_constexpr e2
  let exp2_int = case exp2 of
                   (Exp_Int i) -> i
                   e -> error $ "cannot evaluate " ++ (show e) ++ " in a constant expression"
  return $ Exp_Int $ exp1_int * exp2_int
eval_constexpr (Exp_String s) = do
  val <- get_constexpr s
  case val of
    Just (typename, value) -> return value
    Nothing -> error $ "variable " ++ s ++ " is not a constant expression"
eval_constexpr e = error $ "cannot evaluate expression \n" ++ (print_exp 0 e) ++ "in a compile time context"

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
    (Nothing) -> error $ "variable " ++ varname ++ " does not exist as lvalue"
compile_lvalue arrindexexp@(Exp_ArrIndex lvalue index) = do
  lvalue_type <- compile_lvalue lvalue
  case lvalue_type of
    Type_Arr subtype sublen -> do
      put_instr $ Push $ rax
      index_val <- compile index
      subsize <- sizeof subtype
      put_instrs $
        [ Mov rbx (Literal $ subsize),
          Mul rbx,
          Mov rbx rax,
          Pop $ rax,
          -- Lea rax (Addr $ ("rax [rbx]"))
          Add rax rbx
        ]
      return $ subtype
    Type_Pointer subtype -> do
      subsize <- sizeof subtype
      put_instr $ Mov rax (Addr "QWORD [rax]")
      put_instr $ Push $ rax
      index_val <- compile index
      put_instrs $
        [ Mov rbx (Literal $ subsize),
          Mul rbx,
          Mov rbx rax,
          Pop rax,
          -- Lea rax (Addr $ ("rax [rbx]"))
          Add rax rbx
        ]
      return $ subtype
    o -> error $ "cannot take index of type " ++ (show o) ++ " in expression\n" ++ (print_exp 0 arrindexexp)
compile_lvalue e = error $ "expression \n" ++ (print_exp 0 e) ++ "is not allowed in lhs right now"

compile :: Expression -> Compiler Type
compile (Exp_Int i) = (put_instrs $ [Mov rax (Literal i)]) >> (return Type_I64)
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
    (Just (Sym_Variable pos Type_I32)) -> do
      put_instrs $
        [Mov rbx $ Literal 0, Mov ebx (Addr $ ("DWORD [rbp-" ++ (show pos) ++ "]")), Mov rax rbx]
      return Type_I32
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
    (Nothing) -> do
      constexpr <- get_constexpr varname
      case constexpr of
        Just (typename, value) -> 
          case value of
            (Exp_Int i) -> do
              put_instr $ Mov rax $ Literal i
              return typename
        Nothing -> error $ "variable " ++ varname ++ " does not exist as rvalue"
compile arrindex@(Exp_ArrIndex arr index) = do
  lvalue_type <- compile_lvalue arrindex
  case lvalue_type of
    Type_I64 -> put_instr $ Mov rax (Addr "QWORD [rax]")
    Type_Pointer _ -> put_instr $ Mov rax (Addr "QWORD [rax]")
    Type_Arr _ _ -> put_instr $ Mov rax (Addr "QWORD [rax]")
    Type_I8 -> do
      put_instr $ Mov rbx $ Literal 0
      put_instr $ Mov bl (Addr "BYTE [rax]")
      put_instr $ Mov rax rbx
    Type_I32 -> do
      put_instr $ Mov rbx $ Literal 0
      put_instr $ Mov ebx (Addr "DWORD [rax]")
      put_instr $ Mov rax rbx
  return lvalue_type
compile (Exp_AddressOf s) = Type_Pointer <$> compile_lvalue s
compile (Exp_Plus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      put_instr $ Add rax (Literal i)
      return a
    _ -> do
      put_instr $ Push rax
      b <- compile e2
      if (a == b || (a == Type_I64 && b == Type_I8) || (a == Type_I8 && b == Type_I64) || (good_types a b)) then return () else error $ "operator (+): wrong arguments of type " ++ (show a) ++ " and " ++ (show b)
      put_instrs $
        [Pop rbx] <>
        [Add rax rbx]
      return a
        where
          good_types (Type_Pointer _) (Type_I64) = True
          good_types a b = False
compile (Exp_Minus e1 e2) = do
  a <- compile e1
  case e2 of
    (Exp_Int i) -> do
      put_instr $ Sub rax (Literal i)
      return a
    _ -> do
      put_instr $ Push rax
      b <- compile e2
      if (a == b || (a == Type_I64 && b == Type_I8) || (a == Type_I8 && b == Type_I64) || (good_types a b)) then return () else error $ "operator (-): wrong arguments of type " ++ (show a) ++ " and " ++ (show b)
      put_instrs $
        [Mov rbx rax] <>
        [Pop rax] <>
        [Sub rax rbx]
      return a
        where
          good_types (Type_Pointer _) (Type_I64) = True
          good_types (Type_Pointer _) (Type_Pointer _) = True
          good_types a b = False
compile (Exp_Mult e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  if ((a `elem` int_types) && (b `elem` int_types) && a `type_can_be` b) then return () else error $ "operator (*): wrong arguments of type " ++ (show a) ++ " and " ++ (show b)
  put_instrs $
    [Pop $ rbx] <>
    [Mul rbx]
  return a
compile (Exp_Div e1 e2) = do
  a <- compile e1
  put_instr $ Push rax
  b <- compile e2
  if ((a `elem` int_types) && (b `elem` int_types) && a `type_can_be` b) then return () else error $ "operator (/): wrong arguments of type " ++ (show a) ++ " and " ++ (show b)
  put_instrs $
    [Mov rbx rax] <>
    [Pop $ rax] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div rbx]
  return a
compile (Exp_Mod e1 e2) = do
  a <- compile e1
  put_instr $ Push rax
  b <- compile e2
  if ((a `elem` int_types) && (b `elem` int_types) && a `type_can_be` b) then return () else error $ "operator (%): wrong arguments of type " ++ (show a) ++ " and " ++ (show b)
  put_instrs $
    [Mov rbx rax] <>
    [Pop $ rax] <>
    [Mov (Register "rdx") (Literal 0)] <>
    [Div rbx] <>
    [Mov rax (Register "rdx")]
  return a

compile (Exp_Assignment left_exp right_exp) = do
  lhs <- compile_lvalue left_exp
  put_instr $ Push $ rax
  rhs <- compile right_exp
  put_instr $ Mov rbx rax
  put_instr $ Pop $ rax
  lhssize <- sizeof lhs
  case lhssize of
    1 -> put_instr $ Mov (Addr $ "BYTE [rax]") bl
    4 -> put_instr $ Mov (Addr $ "DWORD [rax]") ebx
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
compile (Exp_NotEquality e1 e2) = do
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
      Jne $ label1,
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

compile (Exp_BitAnd e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  put_instrs $
    [Pop $ rbx] <>
    [And rax rbx]
  return Type_I64
compile (Exp_BitOr e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  put_instrs $
    [Pop $ rbx] <>
    [Or rax rbx]
  return Type_I64
compile (Exp_BitXor e1 e2) = do
  a <- compile e1
  put_instr $ Push $ rax
  b <- compile e2
  put_instrs $
    [Pop $ rbx] <>
    [Xor rax rbx]
  return Type_I64

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

compile (Exp_For init_exp cond_exp final_exp (Exp_SourceBlock body_exps)) = compile (Exp_SourceBlock [init_exp, (Exp_While cond_exp $ Exp_SourceBlock $ body_exps ++ [final_exp])])

compile (Exp_Declaration varname typename_uncanon rhs_exp) = do
  typename <- canonicalize_type typename_uncanon
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
        (Type_I32) -> do
          state <- get_state
          let pos = state.rsp + 4
          put_state $ state { symtab = Map.insert varname (Sym_Variable pos typename) state.symtab, rsp = pos }
          compile rhs_exp
          put_instrs $ [ Sub rsp $ Literal 4, Mov (Addr $ "DWORD [rsp]") eax ]
        (Type_Arr subtype constexpr_length) -> do
          length_val <- eval_constexpr constexpr_length
          let length = case length_val of
                         (Exp_Int a) -> a
                         e -> error $ "unreachable"
          state <- get_state
          subsize <- sizeof subtype
          let arr_size = subsize * length
          put_state $ state { rsp = state.rsp + arr_size, symtab = Map.insert varname (Sym_Variable (state.rsp + arr_size) (Type_Arr subtype (Exp_Int subsize))) state.symtab }
          put_instrs $ [ Sub rsp (Literal $ length * subsize) ]
      return Type_Empty

compile (Exp_Proc (Exp_String name) args body rettype) = do
  let functype = Sym_Function name (map snd args) rettype
  modify_symtab $ Map.insert name functype
  put_instrs $ [ Label name, Push $ rbp, Mov rbp rsp ]
  old_state <- get_state
  push_args args args_registers
  increment_counter
  old_proc <- get_proc
  cur_proc <- get_counter
  put_proc cur_proc
  
  body_instrs <- compile body
  
  put_proc old_proc
  new_state <- get_state
  put_state $ new_state { rsp = old_state.rsp, symtab = old_state.symtab }
  -- put_instrs $ [ Label $ "PROCEND" ++ show cur_proc, Add rsp (Literal $ 8 * (length args)) ] <> [ Pop rbp, Ret ]
  put_instrs $ [ Label $ "PROCEND" ++ show cur_proc, Mov rsp rbp ] <> [ Pop rbp, Ret ]
  return Type_Empty
    where
      args_registers :: [Operand]
      args_registers = take (length args) $ Register <$> register_list

      push_args :: [(String, Type)] -> [Operand] -> Compiler ()
      push_args [] [] = return ()
      push_args ((arg_name, typename):args) (reg:regs) = do
        size <- sizeof typename
        modify_state $ \st -> st { rsp = st.rsp + 8, symtab = Map.insert arg_name (Sym_Variable (st.rsp + 8) typename) st.symtab }
        -- put_instr $ Mov (Addr "QWORD [rsp-8]") reg
        put_instr $ Sub rsp $ Literal 8
        put_instr $ Mov (Addr "QWORD [rsp]") reg
        push_args args regs

compile proccallexp@(Exp_ProcCall procname args) = do
  functype <- compile procname
  let (Type_Func params_type res_type) = functype
  if (length params_type /= length args) then error $ "expected " ++ (show $ length params_type) ++ " parameters but got " ++ (show $ length args) else return ()
  if (length args > 7) then error $ "can pass a maximum of 7 arguments to a procedure (for now)" else return ()
  put_instr $ Push $ rax
  process_args args params_type
  
  put_instrs $ (Pop <$> reverse args_registers) 
  put_instr $ Pop $ rax
  put_instr $ Call $ rax
  return res_type
    where
      args_registers = take (length args) $ Register <$> register_list
      process_args :: [Expression] -> [Type] -> Compiler ()
      process_args [] [] = return ()
      process_args s [] = error $ "extra parameters in proc call: " ++ (show s)
      process_args [] s = error $ "not enough parameters in proc call: " ++ (show s)
      process_args (exp:exprs) (paramtype_uncanon:params) = do
        paramtype <- canonicalize_type paramtype_uncanon
        curparamtype_uncanon <- compile exp
        curparamtype <- canonicalize_type curparamtype_uncanon
        if (curparamtype `type_can_be` paramtype) then (return ()) else error $ ("wrong paramter type: expected " ++ (show paramtype) ++ " but got " ++ (show curparamtype) ++ " in: \n" ++ (print_exp 0 proccallexp))
        modify_state $ (\st -> st { rsp = st.rsp + 8 })
        put_instr $ Push $ rax
        process_args exprs params
        modify_state $ (\st -> st { rsp = st.rsp - 8 })

compile (Exp_Return exp) = do
  block <- get_proc
  compile exp
  put_instr $ Jmp $ "PROCEND" ++ (show block)
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
  put_instrs $ [ Mov rax $ Literal 0, Label $ "BLOCKEND" ++ (show block), Add rsp (Literal $ new_state.rsp - old_state.rsp) ]
  return Type_Empty
    where
      compile_exprs :: [Expression] -> Compiler ()
      compile_exprs = foldr ((>>) . (>> put_instr EmptyLine) . compile) $ pure ()

compile (Exp_Empty) = return Type_Empty

compile e = error $ "unhandled expression in compiler: \n" ++ (show e)

sourceCompiler :: [Expression] -> ([Instruction], [(String, String)], [String])
sourceCompiler exprs = (compile_final_state.instructions, compile_final_state.bss, Set.toList $ compile_final_state.procs)
  where
    compiled = compile_all exprs
    compile_all :: [Expression] -> Compiler ()
    compile_all = foldr (\ e c -> compile_one e >> c) $ pure ()

    compile_one :: Expression -> Compiler ()
    compile_one (Exp_ConstDeclaration name typename val) = do
      put_contexpr name typename val
      return ()
    compile_one e@(Exp_Proc (Exp_String name) args body rettype) = do
      let functype = Sym_Function name (map snd args) rettype
      add_to_modul functype
      compile e
      return ()
    compile_one e = error $ "impossible expression in top level: " ++ (show e)
    
    compile_final_state = fst $ compiled.run initialCompilerState
