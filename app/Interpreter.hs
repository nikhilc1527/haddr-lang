module Interpreter where

import qualified Data.HashMap as HashMap
import qualified Data.STRef as STRef

import Lexer
import Parser

data Value =
  VAL_NUM Integer |
  VAL_STRING String |
  VAL_BOOL Bool |
  VAL_EMPTY
  deriving (Eq, Show)

-- mutable_map :: STRef.STRef (HashMap.Map Int Expression)
-- mutable_map = STRef.newSTRef $ HashMap.empty

-- interpret_statements :: [Expression] -> Value

interpret_statement :: Expression -> Value
interpret_statement (EXP_IF cond expr_true expr_false) =
  case cond_val of
    VAL_NUM value ->
      case value of
        0 -> expr_false_val
        _ -> expr_true_val
    VAL_BOOL value ->
      case value of
        False -> expr_false_val
        _ -> expr_true_val
    _ -> error "incompatible types"
  where
    cond_val = interpret_statement cond
    expr_true_val = interpret_statement expr_true
    expr_false_val = interpret_statement expr_false
    
interpret_statement (EXP_PLUS exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> VAL_NUM $ x + y
      _ -> error "mismatched types"
    _ -> error "mismatched types"
  where
    interp1 = interpret_statement exp1
    interp2 = interpret_statement exp2
interpret_statement (EXP_MINUS exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      (
        case interp2 of
          VAL_NUM y -> VAL_NUM $ x - y
          _ -> error "mismatched types"
      )
    _ -> error "mismatched types"
  where
    interp1 = interpret_statement exp1
    interp2 = interpret_statement exp2
interpret_statement (EXP_MULT exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> VAL_NUM $ x * y
      _ -> error "mismatched types"
    _ -> error "mismatched types"
  where
    interp1 = interpret_statement exp1
    interp2 = interpret_statement exp2
interpret_statement (EXP_DIV exp1 exp2) =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> VAL_NUM $ x `div` y
      _ -> error "mismatched types"
    _ -> error "mismatched types"
  where
    interp1 = interpret_statement exp1
    interp2 = interpret_statement exp2
interpret_statement (EXP_VALUE value) =
  case value of
    TOK_LITERALNUM x -> VAL_NUM $ read x
    TOK_LITERALSTRING x -> VAL_STRING x
    _ -> error "invalid type"

interpret_statement _ = VAL_EMPTY
