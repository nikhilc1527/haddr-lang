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
import Lexer

import Control.Monad.State
import Control.Monad
import Control.Applicative

data Operand = Register String | Addr String

data Instruction =
  Mov Operand Operand |
  Add Operand Operand |
  Call Operand |
  Cmp Operand Operand |
  Je Operand |
  Jlt Operand |
  Div Operand Operand |
  Interrupt Int |
  Mul Operand Operand |
  Pop Operand |
  Push Operand |
  Return |
  Sub Operand Operand |
  Syscall

-- type id of the symbol, the string name of the symbol, and the 
type Symbol = (TypeID, String, Int)
type ASMSymbolTable = Map.Map String Symbol

data ASM = ASM { funcs :: [Instruction], bss :: [(String, String)] }

type CompilerFuncType a = (Expression, ASMSymbolTable) -> Maybe (Expression, ASMSymbolTable, a)
newtype Compiler a = Compiler { compile :: CompilerFuncType a }

instance Functor Compiler where
  fmap f (Compiler comp) =
    Compiler $ \(e, st) -> do
      (a,b,c) <- comp (e,st)
      return (a,b,f c)

instance Applicative Compiler where
  pure a = Compiler $ (\(e,st) -> Just (e,st, a))
  a <*> b = Compiler $ \input -> do
    (x1,y1,f) <- a.compile input
    (x2,y2,z) <- b.compile (x1,y1)
    return (x2,y2, f z)

instance Alternative Compiler where
  empty = Compiler $ const Nothing
  (Compiler a) <|> (Compiler b) = Compiler $ \input -> case a input of
                                                        Nothing -> b input
                                                        Just c -> Just c


