module Lexer where

import Data.STRef
import Control.Monad.ST
import Data.Char

-- START_TOKENS
data TokenType =
  -- tokens that take a single character
  TOK_USERDEF String    | -- a user defined function or variable name
  TOK_LITERALINT Int    | -- literal (4 bytes)
  TOK_CURLYBRACKETOPEN  | -- for defining value from a statement
  TOK_CURLYBRACKETCLOSE |
  TOK_PARENOPEN         | -- for function calls
  TOK_PARENCLOSE        |
  TOK_EQUALS        |
  -- tokens that take multiple characters
  TOK_SYSCALL String    | -- for syscalls, use : and then name of syscall
  TOK_IF                | -- if statement
  TOK_THEN              |
  TOK_ELSE              |
  TOK_END               | -- keyword for ending a for or while
  TOK_ADDR String              | -- operator to get address of value (&)
  TOK_LITERALSTRING String -- literal string with ""
-- END_TOKENS
  deriving (Show)

numTokens :: IO Int
numTokens = do
  let progName = "app/Lexer.hs"
  tokens <- readFile progName >>= (return . drop 2 . takeWhile (\ x -> x /= "-- END_TOKENS") . dropWhile (\ x -> x /= "-- START_TOKENS") . lines)
  return $ length tokens

-- pass in the source code and get returned a list of all of the tokens
getTokens :: String -> [TokenType]
getTokens [] = []
getTokens (first_char:rest)
  | isAlpha first_char || first_char == '_' = (TOK_USERDEF firstword):rest_words_tokens
  | isDigit first_char = (TOK_LITERALINT firstnum):restnum
  | isSpace first_char = rest_tokens
  | first_char == ':' = (TOK_SYSCALL firstword):rest_words_tokens
  | first_char == '&' = (TOK_ADDR firstword):rest_words_tokens
  | first_char == '{' = TOK_CURLYBRACKETOPEN:rest_tokens
  | first_char == '}' = TOK_CURLYBRACKETCLOSE:rest_tokens
  | first_char == '(' = TOK_PARENOPEN:rest_tokens
  | first_char == ')' = TOK_PARENCLOSE:rest_tokens
  | first_char == '=' = TOK_EQUALS:rest_tokens
  | first_char == 'i' && firstword == "f" = TOK_IF:rest_words_tokens
  | first_char == 't' && firstword == "hen" = TOK_THEN:rest_words_tokens
  | first_char == 'e' && firstword == "lse" = TOK_ELSE:rest_words_tokens
  | first_char == 'e' && firstword == "nd" = TOK_END:rest_words_tokens
  | first_char == '"' = (TOK_LITERALSTRING curstring):(getTokens afterstring)
  where
    word_separator = length $ takeWhile (\ x -> isAlpha x || x == '_') rest
    num_separator = length $ takeWhile (isDigit) rest
    
    firstword = [first_char] ++ (take word_separator rest)
    restwords = drop word_separator rest

    firstnum = read $ ([first_char] ++ (take num_separator rest))
    restnum = getTokens $ drop num_separator rest

    rest_tokens = getTokens rest
    rest_words_tokens = getTokens restwords
    
    curstring = "\"" ++ takeWhile (/= '"') rest ++ "\""
    afterstring = tail $ dropWhile (/= '"') rest

getTokens _ = error "you didnt implement lexer for all the token types"
