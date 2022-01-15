{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Data.Bool
import Data.List
import Data.Char
import Data.Data
import Data.List.Split
import qualified Debug.Trace as Trace
import Data.Hashable
import Text.Printf
import qualified Data.HashMap as Map
import Control.Applicative
import Data.Either hiding (show)

data Type = 
  Type_Int
  | Type_String
  | Type_Bool
  | Type_Pointer
  | Type_Func

-- type ExprID = Int
-- data Expression = Expression ExprID [Expression] [TokenType]
--   deriving (Eq, Ord, Show)

data Eq i => Error i
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i Int -- We didn't expect to find this element
  | Expected i [i] Int -- Expected <first i> but got <second i>
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq)

instance (Show i, Eq i) => Show (Error i) where
  show EndOfInput = "end of input\n"
  show (Unexpected i j) = "unexpected: " ++ (show i) ++ " at position " ++ (show j) ++ "\n"
  show (Expected i j k) = "expected: " ++ (show i) ++ ", but got " ++ (show j) ++ " at position " ++ (show k) ++ "\n"

instance Eq i => Ord (Error i) where
  (EndOfInput) <= b = False
  b <= EndOfInput = True
  Empty <= b = True
  b <= Empty = False

  (Unexpected i a) <= (Unexpected j b) = a < b
  (Expected i k a) <= (Unexpected j b) = a < b
  (Unexpected i a) <= (Expected j l b) = a < b
  (Expected i k a) <= (Expected j l b) = a < b

data Input i = Input
  { str :: [i],
    pos :: Int
  } deriving (Show, Eq)

newtype Parser i a = Parser
  { run :: Input i -> Either (Error i) (a, Input i)
  }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    return (f output, rest)

instance Applicative (Parser i) where
  pure a = Parser $ \is -> Right (a, is)
  (Parser a) <*> (Parser b) = Parser $ \input -> do
    (f, r1) <- a input
    (x, r2) <- b r1
    return (f x, r2)

instance (Eq i) => Alternative (Parser i) where
  empty = Parser $ const $ Left $ Empty

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ max err err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

instance Monad (Parser i) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    (k output).run rest

eofP :: Eq i => a -> Parser i a
eofP a = Parser $ \input ->
  case input.str of
    [] -> Right (a, input)
    (b:r) -> Left $ Unexpected b input.pos

satisfyP :: Eq i => (i -> Bool) -> Parser i i
satisfyP pred = Parser $ \input ->
  case input.str of
    [] -> Left EndOfInput
    hd : rest
      | pred hd -> Right (hd, input { str = rest, pos = input.pos + 1 } )
      | otherwise    -> Left $ Unexpected hd input.pos

charP :: Char -> Parser Char Char 
charP i = Parser $ \input ->
  case input.str of
    [] -> Left $ Expected i "EOF" input.pos
    hd : rest
      | hd == i -> Right (hd, input { str = rest, pos = input.pos + 1 } )
      | otherwise    -> Left $ Expected i [hd] input.pos

wcharP = wss . charP

stringP :: String -> Parser Char String
stringP = sequenceA . map charP

spanP :: Eq i => (i -> Bool) -> Parser i [i]
spanP pred = Parser $ \input ->
  let (a, b) = span (pred) input.str
  in case a of
       [] -> case b of
              [] -> Left $ EndOfInput
              (x:xs) -> Left $ Unexpected x input.pos
       _ -> Right (a, input { str = b, pos = input.pos+(length a) } )

data Expression =
  Exp_If Expression Expression Expression |
  Exp_While Expression Expression |
  Exp_Func Expression [Expression] Expression |
  Exp_Call Expression [Expression] |
  Exp_Plus Expression Expression |
  Exp_Minus Expression Expression |
  Exp_Mult Expression Expression |
  Exp_Div Expression Expression |
  Exp_Mod Expression Expression |
  Exp_Equality Expression Expression |
  Exp_LessThan Expression Expression |
  Exp_GreaterThan Expression Expression |
  Exp_And Expression Expression |
  Exp_Or Expression Expression |
  Exp_Dump Expression |
  Exp_Assignment Expression Expression |
  Exp_Declaration String String Expression |
  Exp_ArrIndex Expression Expression |
  Exp_ArrCreate Expression |
  Exp_Value Expression |
  Exp_Int Int |
  Exp_Float Float |
  Exp_String String |
  Exp_SourceBlock [Expression] |
  Exp_Empty |
  Exp_INVALID
  deriving (Eq)

instance Show Expression where
  show = print_exp 0

sp :: Int -> String
sp = (flip replicate) ' '
print_exp :: Int -> Expression -> String
print_exp spaces (Exp_If cond exp_true exp_false) = (sp spaces) ++ "IF\n" ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) exp_true) ++ (print_exp (spaces+2) exp_false)
print_exp spaces (Exp_While cond exp) = (sp spaces) ++ "WHILE\n" ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) exp)
print_exp spaces (Exp_Func name args body) = (sp spaces) ++ "FUNC\n" ++ (print_exp (spaces+2) name) ++ (foldr (\ exp str -> (print_exp (spaces+2) exp) ++ str) "" args) ++ (print_exp (spaces+2) body)
print_exp spaces (Exp_Plus e1 e2) = (sp spaces) ++ "PLUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Minus e1 e2) = (sp spaces) ++ "MINUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Mult e1 e2) = (sp spaces) ++ "MULT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Div e1 e2) = (sp spaces) ++ "DIV\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Mod e1 e2) = (sp spaces) ++ "MOD\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_LessThan e1 e2) = (sp spaces) ++ "LT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_GreaterThan e1 e2) = (sp spaces) ++ "GT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_And e1 e2) = (sp spaces) ++ "AND\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Or e1 e2) = (sp spaces) ++ "OR\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Dump e1) = (sp spaces) ++ "DUMP\n" ++ (print_exp (spaces+2) e1)
print_exp spaces (Exp_Assignment e1 e2) = (sp spaces) ++ "ASSIGNMENT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Declaration varname typename e) = (sp spaces) ++ "DECLARATION\n" ++ (sp $ spaces+2) ++ varname ++ "\n" ++ (print_exp (spaces+2) e)
print_exp spaces (Exp_ArrIndex e1 e2) = (sp spaces) ++ "ARR_INDEX\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_ArrCreate e1) = (sp spaces) ++ "ARR_CREATE\n" ++ (print_exp (spaces+2) e1)
print_exp spaces (Exp_Value e1) = (sp spaces) ++ "VALUE\n" ++ (print_exp (spaces+2) e1)
print_exp spaces (Exp_Int i) = (sp spaces) ++ (show i) ++ "\n"
print_exp spaces (Exp_Float i) = (sp spaces) ++ (show i) ++ "\n"
print_exp spaces (Exp_String s) = (sp spaces) ++ (s) ++ "\n"
print_exp spaces (Exp_Empty) = (sp spaces) ++ "EMPTY\n"
print_exp spaces (Exp_SourceBlock es) = (sp spaces) ++ "SOURCE BLOCK\n" ++ foldr (\e str -> (print_exp (spaces+2) e) ++ str) "" es

integerP :: Parser Char Expression
integerP = Exp_Int <$> read <$> spanP isDigit

floatP :: Parser Char Expression
floatP = Trace.trace "" $ do
  a <- spanP isDigit
  charP '.'
  b <- spanP isDigit
  let f = read (a ++ "." ++ b)
  return $ Exp_Float $ f

numP :: Parser Char Expression
numP = (floatP) <|> (integerP)

ws :: Parser Char String
ws = Parser $ \input -> Right $ let (a, b) = span isSpace input.str in (a, Input {str=b, pos=input.pos + (length a)})

tryParse :: a -> Parser i a -> Parser i a
tryParse def parser = Parser $ \input ->
                                 case parser.run input of
                                   Left err -> return (def, input)
                                   Right a -> Right a

wordP :: Parser Char String
wordP = spanP isAlpha

-- white space surround
wss :: Parser Char a -> Parser Char a
wss parser = ws *> parser <* ws

nop :: a -> Parser Char a
nop a = Parser $ \input -> Right $ (a, input)

binaryOperatorP :: (Parser Char Expression) -> [Operator] -> (Parser Char Expression)
binaryOperatorP next_op ops = do
  initial <- next_op
  nexts <- next_parser <|> nop []
  return $ foldl (\ expr (operator, next_operand) -> operator expr next_operand) initial nexts
    where
      binop (Binary s op) = (s, op)
      binop _ = error "expected binary operators"
      operator_func = foldr (<|>) empty $ map (\op -> let (op_str, operator_func) = binop op in const operator_func <$> (wss $ stringP op_str)) ops
      next_parser = do
        operator <- operator_func
        operand <- next_op
        nexts <-  next_parser <|> nop []
        return ((operator, operand):nexts)

sepParserBy :: Parser Char Expression -> Parser Char a -> Parser Char [Expression]
sepParserBy parser separator = do
  first <- parser
  rest <- exprs <|> nop []
  return (first:rest)
  where
    exprs = do
      sep <- separator
      expr <- parser
      rest <- exprs <|> nop []
      return $ (expr:rest)  

ifP :: Parser Char Expression
ifP = do
  wss $ stringP "if"
  wcharP '('
  cond <- expressionP
  wcharP ')'
  inside <- parseBlock <|> statementP
  else_block <- else_parser <|> nop Exp_Empty
  return $ Exp_If cond inside else_block
    where
      else_parser = do
        wss $ stringP "else"
        inside <- parseBlock <|> statementP
        return inside

whileP :: Parser Char Expression
whileP = do
  wss $ stringP "while"
  wcharP '('
  cond <- expressionP
  wcharP ')'
  body <- parseBlock <|> statementP
  return $ Exp_While cond body

procP :: Parser Char Expression
procP = do
  ws *> stringP "proc"
  satisfyP isSpace
  func_name <- wss $ (Exp_String <$> wordP)
  args <- argsP
  body <- parseBlock <|> statementP
  return $ Exp_Func func_name args body
    where
      argsP :: Parser Char [Expression]
      argsP = (wcharP '(') *> ((const [] <$> (wcharP ')') <|> (
        do
          cur <- Exp_String <$> wordP
          rest <- argsP <|> (const [] <$> (wcharP ')'))
          return $ (cur:rest)
        )))

parseBlock :: Parser Char Expression
parseBlock = Exp_SourceBlock <$> (wcharP '{' *> block)
    where
      block = do
        cur <- controlStructureP <|> (expressionP <* (wcharP ';'))
        rest <- block <|> (const [] <$> (wcharP '}'))
        return $ (cur:rest)

statementP :: Parser Char Expression
statementP = (controlStructureP <|> (expressionP <* (wcharP ';')))

controlStructureP :: Parser Char Expression
controlStructureP = ifP <|> whileP <|> procP

declarationP :: Parser Char Expression
declarationP = do
  varname <- wordP
  wcharP ':'
  typename <- wordP
  wcharP '='
  exp <- expressionP
  return $ Exp_Declaration varname typename exp

data Operator = 
  PrefixUnary String (Expression -> Expression) |
  PostfixUnary String (Expression -> Expression) |
  Binary String (Expression -> Expression -> Expression) 

expressionP :: Parser Char Expression
expressionP = do
  head operators
    where
      operators_raw = [
        -- lowest precedence
        [(("="), Exp_Assignment)],
        [("||", Exp_Or)],
        [("&&", Exp_And)],
        [("==", Exp_Equality)],
        [("<", Exp_LessThan), (">", Exp_GreaterThan), ("==", Exp_Equality)],
        [("+", Exp_Plus), ("-", Exp_Minus)],
        [("*", Exp_Mult), ("/", Exp_Div), ("%", Exp_Mod)]
        -- highest precedence
        ]
      operators :: [Parser Char Expression]
      operators = foldr (\ ops_list new_ops -> let op = binaryOperatorP (head new_ops) ops_list in (op:new_ops)) [parseFinal] operators_raw

parseFinal :: Parser Char Expression
parseFinal = numP <|> (Exp_String <$> wordP) <|> parensP
  where
    parensP = do
      wcharP '('
      exp <- expressionP
      wcharP ')'
      return exp

sourceFileParser :: Parser Char [Expression]
sourceFileParser = do
  (wss $ eofP []) <|> do
    cur <- procP <|> declarationP
    next <- sourceFileParser
    return (cur:next)

runParser :: Parser Char Expression -> String -> IO ()
runParser parser input = putStrLn $ case parser.run $ Input {str=input, pos=0} of
                           Right (exp, rest) -> ((show exp) ++ "\nrest:\n" ++ (show rest))
                           Left err -> show err

