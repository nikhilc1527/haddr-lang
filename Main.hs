#!/bin/sh ghc

module Main where

import System.IO
import System.Environment
import Data.Trie

-- START_TOKENS
data Token =
  SYSCALL           | -- for syscalls, use : and then name of syscall
  FUNC              | -- a user defined function name
  VAL               | -- a user defined variable name
  PTR               | -- a user defined pointer to heap space
  LITERALINT        | -- literal (4 bytes)
  LITERALSTRING     | -- literal string with ""
  IF                | -- if statement
  THEN              |
  ELSE              |
  CURLYBRACKETOPEN  | -- for defining value from a statement
  CURLYBRACKETCLOSE |
  PARENOPEN         | -- for function calls
  PARENCLOSE
-- END_TOKENS

numTokens :: IO Int
numTokens = do
  progName <- getProgName
  tokens <- readFile (progName ++ ".hs") >>= (return . drop 2 . takeWhile (\ x -> x /= "-- END_TOKENS") . dropWhile (\ x -> x /= "-- START_TOKENS") . lines)
  return $ length tokens

main :: IO()
main = do
  numTokens >>= print
