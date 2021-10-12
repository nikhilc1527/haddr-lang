module Parser where

import Lexer

data Expression =
  EXP_IF Expression TokenType Expression TokenType Expression TokenType | -- if expression then expression else expression
  EXP_WHILE Expression TokenType Expression TokenType                   | -- while expression expression
  EXP_BINARY Expression Expression                                      | -- expression for binary operators (+, -, mod, etc)
  EXP_UNARY Expression                                                  | -- expression for unary operators (none as of now)
  EXP_FUNC TokenType [TokenType] | -- the function is just an expression for a name and a list of arguments separated by spaces
  EXP_OPERAND TokenType | -- the function is just an expression for a name and a list of arguments separated by spaces
  EXP_ASSIGNMENT Expression TokenType Expression -- the assignment is an expression on one side (could be either just a variable name or could be a function type)

parseTokens :: [[TokenType]] -> [Expression]
parseTokens = undefined

parseStatement :: [TokenType] -> [Expression]
parseStatement = undefined
