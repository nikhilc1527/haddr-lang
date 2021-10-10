module Main where
import System.IO
import System.Environment

myReadFile :: Handle -> IO String
myReadFile h = do
  eof <- hIsEOF h
  if eof
  then do
    return ""
  else do
    line <- hGetLine h
    nextLines <- myReadFile h
    return $ line ++ "\n" ++ nextLines

main :: IO()
main = do
  fileInput <- getArgs >>= (return . head)
  contents <- withFile fileInput ReadMode myReadFile
  putStrLn contents
