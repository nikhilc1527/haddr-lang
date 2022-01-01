module Lexer where

import Data.STRef
import Control.Monad.ST
import Data.Char
import Text.Printf
import qualified Data.HashMap as Map
import qualified Debug.Trace as Trace
import Data.Hashable

data TokenType =
  -- non terminal tokens - will be used in the parser
  TOK_USERDEF String     | -- a user defined function or variable name
  TOK_LITERALNUM String  | -- literal (4 bytes)  
  -- tokens that take a single character
  TOK_CURLYBRACKETOPEN   | -- for defining value from a statement
  TOK_CURLYBRACKETCLOSE  |
  TOK_SQUAREBRACKETOPEN  |
  TOK_SQUAREBRACKETCLOSE |
  TOK_PARENOPEN          | -- for function calls
  TOK_PARENCLOSE         |
  TOK_EQUALS             |
  TOK_PLUS               |
  TOK_MINUS              |
  TOK_MULT               |
  TOK_DIV                |
  TOK_MOD                |
  TOK_DOUBLE_EQUALS      |
  TOK_LT                 |
  TOK_GT                 |
  TOK_AND                |
  TOK_OR                 |
  TOK_DUMP               |
  TOK_OPERATOR           |
  TOK_SEMICOLON          |
  TOK_COMMA              |
  -- tokens that take multiple characters
  TOK_SYSCALL String     | -- for syscalls, use : and then name of syscall
  TOK_FUNC               | -- function statement
  TOK_IF                 | -- if statement
  TOK_WHILE              | -- while loop
  TOK_DO                 |
  TOK_ELSE               |
  TOK_END                | -- keyword for ending a for or while
  TOK_ADDR String        | -- operator to get address of value (&)
  TOK_LITERALSTRING String -- literal string with ""
  deriving (Eq, Show, Ord)


-- pass in the source code and get returned a list of all of the tokens
getTokens :: String -> [TokenType]
getTokens [] = []
getTokens (first_char:rest)
  | firstsep == "//"                         = getTokens $ tail $ dropWhile (/= '\n') rest
  | isSpace first_char                      = rest_tokens
  | first_char == ':'                        = (TOK_SYSCALL firstword):rest_words_tokens
  | first_char == '{'                        = TOK_CURLYBRACKETOPEN:rest_tokens
  | first_char == '}'                        = TOK_CURLYBRACKETCLOSE:rest_tokens
  | first_char == '['                        = TOK_SQUAREBRACKETOPEN:rest_tokens
  | first_char == ']'                        = TOK_SQUAREBRACKETCLOSE:rest_tokens
  | first_char == '('                        = TOK_PARENOPEN:rest_tokens
  | first_char == ')'                        = TOK_PARENCLOSE:rest_tokens
  | first_char == ';'                        = TOK_SEMICOLON:rest_tokens
  | firstsep   == "=="                       = TOK_DOUBLE_EQUALS:after_space_tokens
  | first_char == '='                        = TOK_EQUALS:rest_tokens
  | first_char == '+'                        = TOK_PLUS:rest_tokens
  | first_char == '-'                        = TOK_MINUS:rest_tokens
  | first_char == '*'                        = TOK_MULT:rest_tokens
  | first_char == '/'                        = TOK_DIV:rest_tokens
  | first_char == '%'                        = TOK_MOD:rest_tokens
  | first_char == '<'                        = TOK_LT:rest_tokens
  | first_char == '>'                        = TOK_GT:rest_tokens
  | first_char == ','                        = TOK_COMMA:rest_tokens
  | firstsep   == "&&"                       = TOK_AND:after_space_tokens
  | firstsep   == "||"                       = TOK_OR:after_space_tokens
  | first_char == '&'                        = (TOK_ADDR firstword):rest_words_tokens
  | firstword  == "func"                     = TOK_FUNC:rest_words_tokens
  | firstword  == "if"                       = TOK_IF:rest_words_tokens
  | firstword  == "while"                    = TOK_WHILE:rest_words_tokens
  | firstword  == "do"                       = TOK_DO:rest_words_tokens
  | firstword  == "else"                     = TOK_ELSE:rest_words_tokens
  | firstword  == "end"                      = TOK_END:rest_words_tokens
  | first_char == '"'                        = (TOK_LITERALSTRING curstring):(getTokens afterstring)  
  | firstsep   == "."                        = TOK_DUMP:after_space_tokens
  | isAlpha first_char || first_char == '_'  = (TOK_USERDEF firstword):rest_words_tokens
  | isDigit first_char                       = (TOK_LITERALNUM firstnum):restnum
  | True                                     = error $ printf "undefined token: %s" [first_char]
  where
    space_sep = length $ takeWhile (/= ' ') rest
    firstsep = [first_char] ++ (take space_sep rest)
    after_space_tokens = getTokens $ drop space_sep rest
    
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
 
