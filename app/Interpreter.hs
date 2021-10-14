module Interpreter where

import Lexer
import Parser

data Value =
  VAL_NUM Integer |
  VAL_STRING String |
  VAL_BOOL Bool |
  VAL_EMPTY
  deriving (Eq, Show)

interpret_statement :: Expression -> Value
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

interpret_statement _ = VAL_EMPTY
