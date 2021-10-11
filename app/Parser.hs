module Parser where

import Lexer

data Expression =
  EXP_IF Expression TokenType Expression TokenType Expression TokenType | -- if expression then expression else expression
  EXP_WHILE Expression TokenType Expression TokenType               | -- while expression expression
  EXP_BINARY Expression Expression                          | -- expression for binary operators (+, -, mod, etc)
  EXP_UNARY Expression                                      | --expression for unary operators (none as of now)
  EXP_FUNCDEF TokenType [TokenType] TokenType Expression                | -- definition of function with fn funcname arg1 arg2 = expression
  EXP_VALDEF TokenType TokenType Expression -- definition of a value with val valname = expression
