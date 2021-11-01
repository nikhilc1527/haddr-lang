module Interpreter where

import qualified Data.Map as Map
import qualified Data.STRef as STRef
import qualified Debug.Trace as Trace
import Data.Bool

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
                   Expression -- has to be expID_LHS
                   (TypeID, Bool, Value, Expression) -- id of return type, is it variable(true) or function(false), value of variable (if it is a variable, otherwise VAL_EMPTY for function), the expression that this value is assigned

interpret_block :: SymbolTable -> Expression -> (SymbolTable, Value)
interpret_block symbolTable (Expression expID exprs tokens)
  | expID /= expID_SRCBLOCK = interpret_statement (Expression expID exprs tokens) symbolTable
  | null exprs = (symbolTable, VAL_EMPTY)
  | length exprs == 1 =
    let
      cur = head exprs
      (symbolTable2, val) = interpret_statement cur symbolTable
    in (symbolTable2, val)
  | True =
    let
      cur = head exprs
      rest = tail exprs
      (symbolTable2, value) = interpret_statement cur symbolTable
      rest_interpreted = interpret_block symbolTable2 $ (Expression expID_SRCBLOCK rest [])
    in
      rest_interpreted

statement_interpreter_generator :: (Integer -> Integer -> Integer) -> Expression -> Expression -> SymbolTable -> (SymbolTable, Value)
statement_interpreter_generator func exp1 exp2 symbolTable =
  case interp1 of
    VAL_NUM x ->
      case interp2 of
      VAL_NUM y -> (Map.union symboltable2 symboltable1, VAL_NUM $ func x y)
      _ -> error ("mismatched types: " ++ (show interp2) ++ " coming from " ++ (show exp1) ++ " and " ++ (show exp2))
    _ -> error ("mismatched types: " ++ (show interp1) ++ " coming from " ++ (show exp1) ++ " and " ++ (show exp2))
  where
    (symboltable1, interp1) = interpret_statement exp1 symbolTable
    (symboltable2, interp2) = interpret_statement exp2 symbolTable

interpret_statement :: Expression -> SymbolTable -> (SymbolTable, Value)
interpret_statement (Expression expID exprs tokens) symbolTable
  | expID == expID_SRCBLOCK = 
      let
        (symtab2, val) = interpret_block symbolTable (Expression expID_SRCBLOCK exprs [])
      in
        (Map.intersection symtab2 symbolTable, val)
  | expID == expID_ASSIGNMENT =
      let
        (Expression _ [] tokens) = head exprs
        exp = head $ tail exprs
        (_, exp_interp) = interpret_statement exp symbolTable
        val_or_func = (length tokens) == 1 -- if theres only one token in the lhs, then its a variable, otherwise its a function
        typeID = 0 -- TODO: find out type id from all the types inside of exp
        symbolTable2 = Map.insert (Expression expID_LHS [] tokens) (typeID, val_or_func, exp_interp, exp) $ Map.delete (Expression expID_LHS [] tokens) symbolTable
      in
        (symbolTable2, exp_interp)
  | expID == expID_IF =
      let
        (cond:expr_true:expr_false:[]) = exprs
        (cond_symboltable, cond_val)             = interpret_statement cond symbolTable
        (expr_true_symboltable, expr_true_val)   = interpret_statement expr_true cond_symboltable
        (expr_false_symboltable, expr_false_val) = interpret_statement expr_false cond_symboltable
      in
        case cond_val of
          VAL_NUM value ->
            case value of
              0 -> (expr_false_symboltable, expr_false_val)
              _ -> (expr_true_symboltable, expr_true_val)
          _ -> error "incompatible types"
  | expID == expID_WHILE = 
      let
        (cond:expr:[]) = exprs
        (cond_symboltable, cond_val) = interpret_statement cond symbolTable
        (expr_symboltable, expr_val) = interpret_statement expr cond_symboltable
        whileAgain = interpret_statement (Expression expID_WHILE [cond, expr] []) expr_symboltable -- the only thing thats changing is the updated symbol table
      in
        case cond_val of
          VAL_NUM value ->
            case value of
              0 -> (cond_symboltable, VAL_EMPTY)
              _ -> whileAgain
          _ -> error "incompatible types"
  | expID == expID_VALUE =
    let value = head tokens in
        case value of
          TOK_LITERALNUM x -> (symbolTable, VAL_NUM $ read x)
          TOK_LITERALSTRING x -> (symbolTable, VAL_STRING x)
          TOK_USERDEF x ->
            let
              lookup = Map.lookup (Expression expID_LHS [] [TOK_USERDEF x]) symbolTable
            in
              case lookup of
                Just (typeid, val_or_func, value, expr) -> (symbolTable, value)
                Nothing -> error ("unkown symbol " ++ x)
          _ -> error "invalid type"
  | True =
    let
      (exp1:exp2:[]) = exprs
      operator
        | expID == expID_PLUS = statement_interpreter_generator (+) exp1 exp2 symbolTable
        | expID == expID_MINUS = statement_interpreter_generator (-) exp1 exp2 symbolTable
        | expID == expID_MULT = statement_interpreter_generator (*) exp1 exp2 symbolTable
        | expID == expID_DIV = statement_interpreter_generator div exp1 exp2 symbolTable
        | expID == expID_MOD = statement_interpreter_generator mod exp1 exp2 symbolTable
        | expID == expID_LT = statement_interpreter_generator (\ x y -> bool 0 1 (x < y)) exp1 exp2 symbolTable
        | expID == expID_GT = statement_interpreter_generator (\ x y -> bool 0 1 (x > y)) exp1 exp2 symbolTable
        | expID == expID_AND = statement_interpreter_generator (\ x y -> (abs $ signum $ x) * (abs $ signum $ y)) exp1 exp2 symbolTable
        | expID == expID_OR = statement_interpreter_generator (\ x y -> (abs $ signum $ x) + (abs $ signum $ y)) exp1 exp2 symbolTable
        | True = error ("unhandled expression for interpreting: " ++ (show $ Expression expID exprs tokens))
    in
      operator
