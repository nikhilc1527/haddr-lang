module Main where

import System.IO
import System.Environment

import Lexer

main :: IO()
main = do
  putStr "\n\n\n"
  let sampleCode = ":write (&123){hello_world 123} if(im gud) then {:print \"hello world\"} else die"
  print $ getTokens sampleCode
  putStr "\n\n\n"
