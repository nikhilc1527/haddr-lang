module Parser where

import Lexer

data Expression =
  EXP_IF Expression TokenType Expression TokenType Expression TokenType | -- if expression then expression else expression
  EXP_WHILE Expression TokenType Expression TokenType                   | -- while expression expression
  EXP_BINARY Expression Expression                                      | -- expression for binary operators (+, -, mod, etc)
  EXP_UNARY Expression                                                  | -- expression for unary operators (none as of now)
  EXP_FUNC TokenType [TokenType] | -- the function is just an expression for a name and a list of arguments separated by spaces
  EXP_SIMPLE TokenType | -- the function is just an expression for a name and a list of arguments separated by spaces
  EXP_ASSIGNMENT Expression TokenType Expression -- the assignment is an expression on one side (could be either just a variable name or could be a function type)

parseStatement :: [TokenType] -> [Expression]
parseStatement tokens = undefined

getStatements :: [TokenType] -> [[TokenType]]
getStatements [] = [[]]
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
