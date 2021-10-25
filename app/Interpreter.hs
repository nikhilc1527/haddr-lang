module Interpreter where

import qualified Data.Map as Map
import qualified Data.STRef as STRef
import qualified Debug.Trace as Trace

import Lexer
import Parser

data Value =
  VAL_NUM Integer |
  VAL_STRING String |
  VAL_BOOL Bool |
  VAL_EMPTY
  deriving (Eq, Show)

type TypeID = Int
typeToID :: Map.Map String Int
typeToID = Map.fromList [("Int", 1), ("String", 2), ("Bool", 3)]

type SymbolTable = Map.Map
                   Expression -- has to be EXP_LHS
                   (TypeID, Bool, Value, Expression) -- id of return type, is it variable(true) or function(false), value of variable (if it is a variable, otherwise VAL_EMPTY for function), the expression that this value is assigned

interpret_block :: SymbolTable -> Expression -> Value
interpret_block symbolTable (EXP_SRCBLOCK []) = VAL_EMPTY
interpret_block symbolTable (EXP_SRCBLOCK (cur:[])) = let (symbolTable2, val) = interpret_statement symbolTable cur in Trace.traceShowId val
interpret_block symbolTable (EXP_SRCBLOCK (cur:rest)) = rest_interpreted
  where
    (symbolTable2, value) = interpret_statement symbolTable cur
    rest_interpreted = interpret_block symbolTable2 $ Trace.traceShow value $ EXP_SRCBLOCK rest
interpret_block symbolTable _ = error "can only interpret block given a source block"

interpret_statement :: SymbolTable -> Expression -> (SymbolTable, Value)
interpret_statement symbolTable (EXP_SRCBLOCK x) = (symbolTable, interpret_block symbolTable $ EXP_SRCBLOCK x)
interpret_statement symbolTable (EXP_ASSIGNMENT (EXP_LHS tokens) exp) = (symbolTable2, exp_interp)
  where
    (_, exp_interp) = interpret_statement symbolTable exp
    val_or_func = (length tokens) == 1
    symbolTable2 = Map.insert (EXP_LHS tokens) (0, val_or_func, exp_interp, exp) symbolTable
interpret_statement symbolTable (EXP_IF cond expr_true expr_false) =
  case cond_val of
    VAL_NUM value ->
      case value of
        0 -> (expr_false_symboltable, expr_false_val)
        _ -> (expr_true_symboltable, expr_true_val)
    VAL_BOOL value ->
      case value of
        False -> (expr_false_symboltable, expr_false_val)
        _     -> (expr_true_symboltable, expr_true_val)
    _ -> error "incompatible types"
  where
    (cond_symboltable, cond_val)             = interpret_statement symbolTable cond
    (expr_true_symboltable, expr_true_val)   = interpret_statement symbolTable expr_true
    (expr_false_symboltable, expr_false_val) = interpret_statement symbolTable expr_false

interpret_statement symbolTable (EXP_PLUS exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> (Map.union symboltable2 symboltable1, VAL_NUM $ x + y)
      _ -> error "mismatched types"
    _ -> error "mismatched types"
  where
    (symboltable1, interp1) = interpret_statement symbolTable exp1
    (symboltable2, interp2) = interpret_statement symbolTable exp2
interpret_statement symbolTable (EXP_MINUS exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> (Map.union symboltable2 symboltable1, VAL_NUM $ x - y)
      _ -> error "mismatched types"
    _ -> error "mismatched types"
  where
    (symboltable1, interp1) = interpret_statement symbolTable exp1
    (symboltable2, interp2) = interpret_statement symbolTable exp2
interpret_statement symbolTable (EXP_MULT exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> (Map.union symboltable2 symboltable1, VAL_NUM $ x * y)
      _ -> error "mismatched types"
    _ -> error "mismatched types"
  where
    (symboltable1, interp1) = interpret_statement symbolTable exp1
    (symboltable2, interp2) = interpret_statement symbolTable exp2
interpret_statement symbolTable (EXP_DIV exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> (Map.union symboltable2 symboltable1, VAL_NUM $ x `div` y)
      _ -> error "mismatched types"
    _ -> error "mismatched types"
  where
    (symboltable1, interp1) = interpret_statement symbolTable exp1
    (symboltable2, interp2) = interpret_statement symbolTable exp2
interpret_statement symbolTable (EXP_VALUE value) =
  case value of
    TOK_LITERALNUM x -> (symbolTable, VAL_NUM $ read x)
    TOK_LITERALSTRING x -> (symbolTable, VAL_STRING x)
    TOK_USERDEF x -> let Just (typeid, val_or_func, value, expr) = Map.lookup (EXP_LHS $ [TOK_USERDEF x]) symbolTable in (symbolTable, value)
    _ -> error "invalid type"

interpret_statement symbolTable _ = (symbolTable, VAL_EMPTY)
