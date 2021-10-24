module Lexer where

import Data.STRef
import Control.Monad.ST
import Data.Char
import Text.Printf
import qualified Data.HashMap as Map
import qualified Debug.Trace as Trace

data TokenType =
  -- non terminal tokens - will be used in the parser
  TOK_USERDEF String    | -- a user defined function or variable name
  TOK_LITERALNUM String | -- literal (4 bytes)  
  -- tokens that take a single character
  TOK_CURLYBRACKETOPEN  | -- for defining value from a statement
  TOK_CURLYBRACKETCLOSE |
  TOK_PARENOPEN         | -- for function calls
  TOK_PARENCLOSE        |
  TOK_EQUALS            |
  TOK_PLUS              |
  TOK_MINUS             |
  TOK_MULT              |
  TOK_DIV               |
  TOK_OPERATOR          |
  TOK_SEMICOLON         |
  -- tokens that take multiple characters
  TOK_SYSCALL String    | -- for syscalls, use : and then name of syscall
  TOK_IF                | -- if statement
  TOK_WHILE             | -- while loop
  TOK_DO                |
  TOK_ELSE              |
  TOK_END               | -- keyword for ending a for or while
  TOK_ADDR String       | -- operator to get address of value (&)
  TOK_LITERALSTRING String -- literal string with ""
  deriving (Eq, Show)

-- pass in the source code and get returned a list of all of the tokens
getTokens :: String -> [TokenType]
getTokens [] = []
getTokens (first_char:rest)
  | isSpace first_char                       = rest_tokens
  | first_char == ':'                        = (TOK_SYSCALL firstword):rest_words_tokens
  | first_char == '&'                        = (TOK_ADDR firstword):rest_words_tokens
  | first_char == '{'                        = TOK_CURLYBRACKETOPEN:rest_tokens
  | first_char == '}'                        = TOK_CURLYBRACKETCLOSE:rest_tokens
  | first_char == '('                        = TOK_PARENOPEN:rest_tokens
  | first_char == ')'                        = TOK_PARENCLOSE:rest_tokens
  | first_char == ';'                        = TOK_SEMICOLON:rest_tokens
  | first_char == '='                        = TOK_EQUALS:rest_tokens
  | first_char == '+'                        = TOK_PLUS:rest_tokens
  | first_char == '-'                        = TOK_MINUS:rest_tokens
  | first_char == '*'                        = TOK_MULT:rest_tokens
  | first_char == '/'                        = TOK_DIV:rest_tokens
  | firstword == "if"                        = TOK_IF:rest_words_tokens
  | firstword == "while"                     = TOK_WHILE:rest_words_tokens
  | firstword == "do"                        = TOK_DO:rest_words_tokens
  | firstword == "else"                      = TOK_ELSE:rest_words_tokens
  | firstword == "end"                       = TOK_END:rest_words_tokens
  | first_char == '"'                        = (TOK_LITERALSTRING curstring):(getTokens afterstring)  
  | first_char == '.'                        = error "cannot have a period at the beginning of a number (use 0. if you want to represent a decimal"
  | isAlpha first_char || first_char == '_'  = (TOK_USERDEF firstword):rest_words_tokens
  | isDigit first_char                       = (TOK_LITERALNUM firstnum):restnum
  | True                                     = error $ printf "undefined token: %s" [first_char]
  where
    word_separator     = length $ takeWhile (\ x -> isAlpha x || x == '_') rest
    num_separator      = length $ takeWhile (\ x -> isDigit x || x == '.') rest
    
    firstword          = [first_char] ++ (take word_separator rest)
    restwords          = drop word_separator rest

    firstnum           = [first_char] ++ (take num_separator rest)
    restnum            = getTokens $ drop num_separator rest

    rest_tokens        = getTokens rest
    rest_words_tokens  = getTokens restwords
    
    curstring          = "\"" ++ takeWhile (/= '"') rest ++ "\""
    afterstring        = tail $ dropWhile (/= '"') rest
 
