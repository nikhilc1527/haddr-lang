{-# LANGUAGE DeriveDataTypeable #-}

module Parser where

import Lexer

import Data.Bool
import Data.Data

data Expression =
  EXP_IF Expression TokenType Expression TokenType Expression TokenType | -- if expression then expression else expression
  EXP_WHILE Expression TokenType Expression TokenType                   | -- while expression expression
  EXP_BINARY Expression Expression                                      | -- expression for binary operators (+, -, mod, etc)
  EXP_PLUS Expression Expression                                      | -- expression for binary operators (+, -, mod, etc)
  EXP_MINUS Expression Expression                                      | -- expression for binary operators (+, -, mod, etc)
  EXP_MULT Expression Expression                                      | -- expression for binary operators (+, -, mod, etc)
  EXP_DIV Expression Expression                                      | -- expression for binary operators (+, -, mod, etc)
  EXP_UNARY Expression                                                  | -- expression for unary operators (none as of now)
  EXP_FUNC TokenType [TokenType] | -- the function is just an expression for a name and a list of arguments separated by spaces
  EXP_VALUE TokenType | -- the function is just an expression for a name and a list of arguments separated by spaces
  EXP_ASSIGNMENT Expression TokenType Expression | -- the assignment is an expression on one side (could be either just a variable name or could be a function type)
  EXP_EMPTY
  deriving (Eq, Show)

print_exp :: Int -> Expression -> String
print_exp spaces (EXP_PLUS e1 e2)   = (take spaces $ repeat ' ') ++ "PLUS\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_MINUS e1 e2)  = (take spaces $ repeat ' ') ++ "MINUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_MULT e1 e2)   = (take spaces $ repeat ' ') ++ "MULT\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_DIV e1 e2)    = (take spaces $ repeat ' ') ++ "DIV\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_VALUE t1)     = (take spaces $ repeat ' ') ++ (show t1) ++ "\n"

unwrap_maybe_exp :: Maybe (Expression, [TokenType]) -> (Expression, [TokenType])
unwrap_maybe_exp = maybe (EXP_EMPTY, []) id

parseL0 :: [TokenType] -> Maybe (Expression, [TokenType]) -- returns maybe a syntax tree of expression or nothing if couldnt parse, and if could parse then the 
parseL0 [] = Nothing
parseL0 (TOK_IF:tokens) = Nothing
  -- where
  --   l0_1 = parseL0 tokens

parseL1 :: [TokenType] -> Maybe (Expression, [TokenType])
parseL1 [] = error "[ERROR]: parseL1: empty input"
parseL1 tokens_0
  | l2_1 == EXP_EMPTY           = error "[ERROR]: parseL1: l2_1 is empty"
  | null tokens_1               = Just (l2_1, tokens_1)
  | head tokens_1 == TOK_PLUS   = Just ((EXP_PLUS l1_2 l2_1), tokens_2)
  | head tokens_1 == TOK_MINUS  = Just ((EXP_MINUS l1_2 l2_1), tokens_2)
  | True                        = Just (l2_1, tokens_1)
  where
    (l2_1, tokens_1) = unwrap_maybe_exp $ parseL2 tokens_0
    (l1_2, tokens_2) = unwrap_maybe_exp $ parseL1 $ tail tokens_1

parseL2 :: [TokenType] -> Maybe (Expression, [TokenType])
parseL2 [] = error "[ERROR]: parseL2: empty input"
parseL2 tokens_0
  | l3_1 == EXP_EMPTY         = error "[ERROR]: parseL2: l2_1 is empty"
  | null tokens_1             = Just (l3_1, tokens_1)
  | head tokens_1 == TOK_MULT = Just ((EXP_MULT l2_2 l3_1), tokens_2)
  | head tokens_1 == TOK_DIV  = Just ((EXP_DIV l2_2 l3_1), tokens_2)
  | True                      = Just (l3_1, tokens_1)
  where
    (l3_1, tokens_1) = unwrap_maybe_exp $ parseL3 tokens_0
    (l2_2, tokens_2) = unwrap_maybe_exp $ parseL2 $ tail tokens_1

parseL3 :: [TokenType] -> Maybe (Expression, [TokenType])
parseL3 [] = error "[ERROR]: parseL3: empty input"
parseL3 (TOK_USERDEF s:tokens_0) = Just (EXP_VALUE $ TOK_USERDEF s, tokens_0)
parseL3 (TOK_LITERALNUM s:tokens_0) = Just (EXP_VALUE $ TOK_LITERALNUM s, tokens_0)
parseL3 (TOK_LITERALSTRING s:tokens_0) = Just (EXP_VALUE $ TOK_LITERALSTRING s, tokens_0)

parseL3 tokens_0
  | TOK_PARENOPEN <- head tokens_0 = case l1_1 of
      EXP_EMPTY -> error "[ERROR]: parseL3: empty after open paren"
      _ -> if (null tokens_1) then error "[ERROR]: parseL3: empty after l1_1" else (case head tokens_1 of
                                                   TOK_PARENCLOSE -> Just (l1_1, tail tokens_1)
                                                   _ -> error "[ERROR]: parseL3: no closing paren after l1_1"
                                                )
  | True = error "[ERROR]: parseL3: non-exhaustive"
  where
    (l1_1, tokens_1) = unwrap_maybe_exp $ parseL1 $ tail tokens_0

parseStatement :: [TokenType] -> [Expression]
parseStatement tokens = undefined

getStatements :: [TokenType] -> [[TokenType]]
getStatements [] = []
getStatements tokens = a:(getStatements b)
  where
    (a, b) = nextStatement tokens 0 0

-- get the next statement delimited by a semicolon
-- parameters: tokens, current paren stack, current brackets stack, (first statement, rest of tokens)
nextStatement :: [TokenType] -> Int -> Int -> ([TokenType],[TokenType])
nextStatement [] p_stack b_stack = ([],[])
nextStatement (TOK_SEMICOLON:rest) 0 0 = ([],rest)
nextStatement (TOK_PARENOPEN:rest) a b = (([TOK_PARENOPEN] ++ first), second)
  where
    (first, second) = nextStatement rest (a+1) b
nextStatement (TOK_PARENCLOSE:rest) a b = (([TOK_PARENCLOSE] ++ first), second)
  where
    (first, second) = nextStatement rest (a-1) b
nextStatement (TOK_CURLYBRACKETOPEN:rest) a b = (([TOK_CURLYBRACKETOPEN] ++ first), second)
  where
    (first, second) = nextStatement rest a (b+1)
nextStatement (TOK_CURLYBRACKETCLOSE:rest) a b = (([TOK_CURLYBRACKETCLOSE] ++ first), second)
  where
    (first, second) = nextStatement rest a (b-1)
nextStatement (cur:rest) a b = (([cur] ++ first), second)
  where
    (first, second) = nextStatement rest a b
