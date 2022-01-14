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

data Type = 
  Type_Int
  | Type_String
  | Type_Bool
  | Type_Pointer
  | Type_Func

-- type ExprID = Int
-- data Expression = Expression ExprID [Expression] [TokenType]
--   deriving (Eq, Ord, Show)

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

data Input i = Input
  { str :: [i],
    pos :: Int
  } deriving (Show, Eq)

newtype Parser i e a = Parser
  { run :: Input i -> Either [Error i e] (a, Input i)
  }

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    return (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \is -> Right (a, is)
  (Parser a) <*> (Parser b) = Parser $ \input -> do
    (f, r1) <- a input
    (x, r2) <- b r1
    return (f x, r2)

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ const $ Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    (k output).run rest

satisfyP :: (i -> Bool) -> Parser i e i
satisfyP pred = Parser $ \input ->
  case input.str of
    [] -> Left [EndOfInput]
    hd : rest
      | pred hd -> Right (hd, input { str = rest, pos = input.pos + 1 } )
      | otherwise    -> Left [Unexpected hd]

charP :: Eq i => i -> Parser i e i
charP i = satisfyP (== i)

stringP :: Eq i => [i] -> Parser i e [i]
stringP = sequenceA . map charP

spanP :: Eq i => (i -> Bool) -> Parser i e [i]
spanP pred = Parser $ \input ->
  let (a, b) = span (pred) input.str
  in case a of
       [] -> case b of
              [] -> Left [EndOfInput]
              (x:xs) -> Left [Unexpected x]
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

integerP :: Parser Char e Expression
integerP = Exp_Int <$> read <$> spanP isDigit

floatP :: Parser Char e Expression
floatP = Trace.trace "" $ do
  a <- spanP isDigit
  charP '.'
  b <- spanP isDigit
  let f = read (a ++ "." ++ b)
  return $ Exp_Float $ f

numP :: Eq e => Parser Char e Expression
numP = (floatP) <|> (integerP)

ws :: Parser Char e String
ws = Parser $ \input -> Right $ let (a, b) = span isSpace input.str in (a, Input {str=b, pos=input.pos + (length a)})

tryParse :: a -> Parser i e a -> Parser i e a
tryParse def parser = Parser $ \input ->
                                 case parser.run input of
                                   Left err -> return (def, input)
                                   Right a -> Right a

wordP :: Parser Char e String
wordP = spanP isAlpha

-- white space surround
wss :: Parser Char e a -> Parser Char e a
wss parser = ws *> parser <* ws

nop :: a -> Parser Char e a
nop a = Parser $ \input -> Right $ (a, input)

type BinOpT = Expression -> Expression -> Expression

binaryOperatorP :: Eq e => (Parser Char e Expression) -> [(String, BinOpT)] -> (Parser Char e Expression)
binaryOperatorP next_op ops = do
  initial <- next_op
  nexts <- next_parser <|> nop []
  return $ foldl (\ expr (operator, next_operand) -> operator expr next_operand) initial nexts
    where
      operator_func = foldr (<|>) empty $ map (\(op_str, operator_func) -> const operator_func <$> (wss $ stringP op_str)) ops
      next_parser = do
        operator <- operator_func
        operand <- next_op
        nexts <-  next_parser <|> nop []
        return ((operator, operand):nexts)

sepParserBy :: Eq e => Parser Char e Expression -> Parser Char e a -> Parser Char e [Expression]
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

ifP :: Eq e => Parser Char e Expression
ifP = do
  wss $ stringP "if"
  wss $ charP '('
  cond <- expressionP
  wss $ charP ')'
  inside <- parseBlock <|> statementP
  else_block <- else_parser <|> nop Exp_Empty
  return $ Exp_If cond inside else_block
    where
      else_parser = do
        wss $ stringP "else"
        inside <- parseBlock <|> statementP
        return inside

whileP :: Eq e => Parser Char e Expression
whileP = do
  wss $ stringP "while"
  wss $ charP '('
  cond <- expressionP
  wss $ charP ')'
  body <- parseBlock <|> statementP
  return $ Exp_While cond body

procP :: Eq e => Parser Char e Expression
procP = do
  ws *> stringP "proc"
  satisfyP isSpace
  func_name <- wss $ (Exp_String <$> wordP)
  wss $ charP '('
  args <- sepParserBy (Exp_String <$> wordP) (wss $ charP ',')
  wss $ charP ')'
  body <- parseBlock <|> statementP
  return $ Exp_Func func_name args body

parseBlock :: Eq e => Parser Char e Expression
parseBlock = Exp_SourceBlock <$> (wss $ charP '{' *> block)
    where
      block = do
        cur <- controlStructureP <|> (expressionP <* (wss $ charP ';'))
        rest <- block <|> (const [] <$> (wss $ charP '}'))
        return $ (cur:rest)

statementP :: Eq e => Parser Char e Expression
statementP = (controlStructureP <|> expressionP)

controlStructureP :: Eq e => Parser Char e Expression
controlStructureP = ifP <|> whileP <|> funcP

declarationP :: Eq e => Parser Char e Expression
declarationP = do
  varname <- wordP
  wss $ charP ':'
  typename <- wordP
  wss $ charP '='
  exp <- expressionP
  return $ Exp_Declaration varname typename exp

expressionP :: Eq e => Parser Char e Expression
expressionP = do
  (head operators) <|> nop Exp_Empty
    where
      operators_raw = [
        -- highest precedence
        [(("="), Exp_Assignment)],
        [("||", Exp_Or)],
        [("&&", Exp_And)],
        [("==", Exp_Equality)],
        [("<", Exp_LessThan), (">", Exp_GreaterThan), ("==", Exp_Equality)],
        [("+", Exp_Plus), ("-", Exp_Minus)],
        [("*", Exp_Mult), ("/", Exp_Div), ("%", Exp_Mod)]
        -- least precedence
        ]
      operators :: Eq e => [Parser Char e Expression]
      operators = foldr (\ ops_list new_ops -> let op = binaryOperatorP (head new_ops) ops_list in (op:new_ops)) [parseFinal] operators_raw

parseFinal :: Eq e => Parser Char e Expression
parseFinal = numP <|> (Exp_String <$> wordP) <|> parensP
  where
    parensP = do
      _ <- ws *> charP '(' <* ws
      exp <- expressionP
      _ <- ws *> charP ')' <* ws
      return exp



runParser :: (Show e, Eq e) => Parser Char e Expression -> String -> IO ()
runParser parser input = putStrLn $ case parser.run $ Input {str=input, pos=0} of
                           Right (exp, rest) -> ((show exp) ++ "\nrest:\n" ++ (show rest))
                           Left err -> show err

