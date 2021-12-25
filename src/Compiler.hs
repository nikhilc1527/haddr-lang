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

import Parser
import Lexer

type ASM = [String]

-- type id of the symbol, the string name of the symbol, and the 
type Symbol = (TypeID, String, Int)
type ASMSymbolTable = Map.Map String Symbol

-- type ASMSymbolTable = Map.Map String (Int, Int)
type Register = String
type BlockDepth = Int
type MaxOffset = Int

compileASM :: ASM -> String
compileASM = undefined

mov :: Register -> Register -> ASM
mov reg1 reg2 = [printf "mov %s, %s\n" reg1 reg2]
push :: String -> ASM
push val = [printf "push %s\n" val]

-- expression to generate asm for, symbol table map, block depth, register to store the result in
exprToASM :: Expression -> ASMSymbolTable -> BlockDepth -> MaxOffset -> Register -> ASM

-- SOURCE BLOCK
exprToASM (Expression expID exprs toks) symtab depth max_offset reg
  | expID == expID_SRCBLOCK =
    concat $ map (\ x -> exprToASM x Map.empty (depth+1) max_offset "rax") $ exprs

-- VALUE
exprToASM (Expression expID exprs toks) symtab depth max_offset reg
  | expID == expID_VALUE =
    case head toks of
      TOK_LITERALNUM x -> mov reg (init $ tail $ show x)
      -- TOK_USERDEF x -> mov (show x) reg

-- PLUS
exprToASM (Expression expID exprs toks) symtab depth max_offset reg
  | expID == expID_PLUS = left_out:right_out:op:mov_out
  where
    left_out = concat $ exprToASM (exprs !! 0) symtab depth max_offset "rax"
    right_out = concat $ exprToASM (exprs !! 1) symtab depth max_offset "rbx"
    op = "add rax, rbx\n"
    mov_out = mov "rax" reg

-- DUMP
exprToASM (Expression expID exprs toks) symtab depth max_offset reg
  | expID == expID_DUMP = child ++ (mov "rdi" reg) ++ ["call printi"]
  where
    child = exprToASM (head exprs) symtab depth max_offset reg
