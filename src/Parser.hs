module Parser where

import Data.Bool
import Data.List
import Data.Char
import Data.Foldable
import Data.Data
import Data.List.Split
import qualified Debug.Trace as Trace
import Data.Hashable
import Text.Printf
import qualified Data.HashMap as Map
import Control.Applicative

uncomment :: String -> String
uncomment [] = []
uncomment input@(_:[]) = input
uncomment ('/':'/':rest) = uncomment $ dropWhile (/= '\n') rest
uncomment (a:rest) = a:(uncomment rest)

data Type = 
  Type_I64 |
  Type_I8 |
  Type_String |
  Type_Bool |
  Type_Pointer Type |
  Type_Arr Type Int |
  Type_Func [Type] Type |
  Type_Empty
  deriving (Eq, Show)

sizeof :: Type -> Int
sizeof (Type_I64) = 8
sizeof (Type_I8) = 1
sizeof (Type_Pointer _) = 8
sizeof (Type_Arr subtype len) = len * (sizeof subtype)

data Eq i => Error i
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i Int -- We didn't expect to find this element
  | Expected i [i] Int -- Expected <first i> but got <second i>
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq)

get_srcpos :: Int -> String -> (Int, Int)
get_srcpos pos str =
  let
    line = length $ filter (== '\n') $ take pos str
    col = length $ takeWhile (/= '\n') $ reverse $ take pos str
  in
    (line+1, col+1)

show_err :: (Show i, Eq i) => Error i -> String -> String
show_err EndOfInput _ = "end of input\n"
show_err (Unexpected i j) orig = let (row, col) = get_srcpos j orig in "unexpected: " ++ (show i) ++ " at position " ++ (show row) ++ ":" ++ (show col) ++ "\n"
show_err (Expected i j k) orig = let (row, col) = get_srcpos k orig in "expected: " ++ (show i) ++ ", but got " ++ (show j) ++ " at position " ++ (show row) ++ ":" ++ (show col) ++ "\n"
show_err Empty _ = undefined

instance (Show i, Eq i) => Show (Error i) where
  show EndOfInput = "end of input\n"
  show (Unexpected i j) = "unexpected: " ++ (show i) ++ " at position " ++ (show j) ++ "\n"
  show (Expected i j k) = "expected: " ++ (show i) ++ ", but got " ++ (show j) ++ " at position " ++ (show k) ++ "\n"
  show Empty = undefined

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

  -- k :: a -> Parser b
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    (k output).run rest

instance Eq i => Semigroup (Parser i a) where
  a <> b = a <|> b
  
instance Eq i => Monoid (Parser i a) where
  mempty = empty

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
  let (a, b) = span pred input.str
  in case a of
       [] -> case b of
              [] -> Left $ EndOfInput
              (x:xs) -> Left $ Unexpected x input.pos
       _ -> Right (a, input { str = b, pos = input.pos+(length a) } )

data Expression =
  Exp_If Expression Expression Expression |
  Exp_While Expression Expression |
  Exp_For Expression Expression Expression Expression |
  Exp_Proc Expression [(String, Type)] Expression Type |
  Exp_ProcCall Expression [Expression] |
  Exp_Call Expression [Expression] |
  Exp_Plus Expression Expression |
  Exp_Minus Expression Expression |
  Exp_Mult Expression Expression |
  Exp_Div Expression Expression |
  Exp_Mod Expression Expression |
  Exp_Comma Expression Expression |
  Exp_Equality Expression Expression |
  Exp_LessEqual Expression Expression |
  Exp_GreaterEqual Expression Expression |
  Exp_LessThan Expression Expression |
  Exp_GreaterThan Expression Expression |
  Exp_And Expression Expression |
  Exp_Or Expression Expression |
  Exp_Assignment Expression Expression |
  Exp_Declaration String Type Expression |
  Exp_ArrIndex Expression Expression |
  Exp_Return Expression |
  Exp_Int Int |
  Exp_Float Float |
  Exp_String String |
  Exp_StringLiteral String |
  Exp_SourceBlock [Expression] |
  Exp_Empty |
  Exp_INVALID
  deriving (Eq, Show)

-- instance Show Expression where
--   show = print_exp 0

sp :: Int -> String
sp = (flip replicate) ' '
print_exp :: Int -> Expression -> String
print_exp spaces (Exp_If cond exp_true exp_false) = (sp spaces) ++ "IF\n" ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) exp_true) ++ (print_exp (spaces+2) exp_false)
print_exp spaces (Exp_While cond exp) = (sp spaces) ++ "WHILE\n" ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) exp)
print_exp spaces (Exp_Proc name args body rettype) = (sp spaces) ++ "PROC\n" ++ (print_exp (spaces+2) name) ++ (foldr (\t s -> (show t) ++ "\n" ++ s) "" args) ++ (print_exp (spaces+2) body) ++ (sp $ spaces+2) ++ (show rettype)
print_exp spaces (Exp_ProcCall name args) = (sp spaces) ++ "PROC CALL\n" ++ (print_exp (spaces+2) name) ++ (foldr (++) "" (map (print_exp $ spaces + 2) args))
print_exp spaces (Exp_Plus e1 e2) = (sp spaces) ++ "PLUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Minus e1 e2) = (sp spaces) ++ "MINUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Mult e1 e2) = (sp spaces) ++ "MULT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Div e1 e2) = (sp spaces) ++ "DIV\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Mod e1 e2) = (sp spaces) ++ "MOD\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Comma e1 e2) = (sp spaces) ++ "COMMA\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_LessThan e1 e2) = (sp spaces) ++ "LT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_GreaterEqual e1 e2) = (sp spaces) ++ "GE\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_LessEqual e1 e2) = (sp spaces) ++ "LE\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_GreaterThan e1 e2) = (sp spaces) ++ "GT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Equality e1 e2) = (sp spaces) ++ "EQUALITY\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_And e1 e2) = (sp spaces) ++ "AND\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Or e1 e2) = (sp spaces) ++ "OR\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Assignment e1 e2) = (sp spaces) ++ "ASSIGNMENT\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Declaration varname typename e) = (sp spaces) ++ "DECLARATION\n" ++ (sp $ spaces+2) ++ varname ++ "\n" ++ (print_exp (spaces+2) e)
print_exp spaces (Exp_ArrIndex e1 e2) = (sp spaces) ++ "ARR_INDEX\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (Exp_Return e) = (sp spaces) ++ "RETURN" ++ "\n" ++ (print_exp (spaces+2) e)
print_exp spaces (Exp_Int i) = (sp spaces) ++ (show i) ++ "\n"
print_exp spaces (Exp_Float i) = (sp spaces) ++ (show i) ++ "\n"
print_exp spaces (Exp_String s) = (sp spaces) ++ s ++ "\n"
print_exp spaces (Exp_StringLiteral s) = (sp spaces) ++ "\"" ++ s ++ "\"\n"
print_exp spaces (Exp_Empty) = (sp spaces) ++ "EMPTY\n"
print_exp spaces (Exp_SourceBlock es) = (sp spaces) ++ "SOURCE BLOCK\n" ++ foldr (\e str -> (print_exp (spaces+2) e) ++ str) "" es
print_exp spaces e = error $ "unexhaustive print_exp: (" ++ (show e) ++ ")"

integerP :: Parser Char Int
integerP = read <$> spanP isDigit

floatP :: Parser Char Float
floatP = do
  a <- spanP isDigit
  charP '.'
  b <- spanP isDigit
  let f = read $ a ++ "." ++ b
  return $ f

numP :: Parser Char Expression
numP = (Exp_Float <$> floatP) <|> (Exp_Int <$> integerP)

charLiteralP :: Parser Char Expression
charLiteralP = (ws *> charP '\'') *> (Exp_Int <$> ord <$> (satisfyP $ const True)) <* (charP '\'' <* ws)

stringLiteralP :: Parser Char Expression
stringLiteralP = do
  ws *> charP '"'
  str <- spanP (/= '"')
  charP '"' <* ws
  return $ Exp_StringLiteral $ str

ws :: Parser Char String
ws = Parser $ \input -> Right $ let (a, b) = span isSpace input.str in (a, input {str=b, pos=input.pos + (length a)})

wordP :: Parser Char String
wordP = spanP (\c -> isAlpha c || c == '_')

-- white space surround
wss :: Parser Char a -> Parser Char a
wss parser = ws *> parser <* ws

nop :: a -> Parser i a
nop a = Parser $ \input -> Right $ (a, input)

notReallyP :: Eq i => i -> Parser i i
notReallyP i = Parser $ \input ->
  case input.str of
    (f:r) | f == i -> Right (f, input)
          | True -> Left $ Expected i [f] input.pos
    [] -> Left EndOfInput

operatorLevelP :: (Parser Char Expression) -> OperatorLevel -> (Parser Char Expression)
-- array subscript and procedure call
operatorLevelP next_op (FixedLevel 1) = do
  next <- next_op
  arr_or_calls <- get_arr_or_calls <|> nop []
  return $ foldl (\lv op -> op lv) next arr_or_calls
-- operatorLevelP next_op (FixedLevel 1) = arrIndexP <|> procCallP <|> next_op
  where
    get_arr_or_calls = do
      cur <- arrIndexP <|> procCallP
      nexts <- get_arr_or_calls <|> nop []
      return $ cur:nexts
    arrIndexP = do
      wcharP '['
      value <- expressionP
      wcharP ']'
      return $ (flip Exp_ArrIndex) value
    procCallP = do
      wcharP '('
      params <- (commas_to_list <$> expressionP) <|> nop []
      wcharP ')'
      return $ (flip Exp_ProcCall) params
        where
          commas_to_list :: Expression -> [Expression]
          commas_to_list (Exp_Comma e1 e2) = ((commas_to_list e1) ++ [e2])
          commas_to_list e = [e]

operatorLevelP next_op bs@(BinaryOperatorList ops) = do
  initial <- next_op
  nexts <- next_parser <|> nop []
  return $ foldl (\ expr (operator, next_operand) -> operator expr next_operand) initial nexts
    where
      operator_func = fold $ map (\(op_str, operator_func) -> const operator_func <$> (wss $ stringP op_str)) ops
      next_parser = do
        operator <- operator_func
        operand <- next_op
        nexts <- next_parser <|> nop []
        return ((operator, operand):nexts)
operatorLevelP _ _ = error "unreachable"

sepParserBy :: Parser Char Expression -> Parser Char a -> Parser Char b -> Parser Char [Expression]
sepParserBy parser separator finisher = do
  first <- parser
  rest <- exprs <|> (const [] <$> finisher)
  return (first:rest)
  where
    exprs = do
      sep <- separator
      expr <- parser
      rest <- exprs <|> (const [] <$> finisher)
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

forP :: Parser Char Expression
forP = do
  wss $ stringP "for"
  wcharP '('
  init <- declarationP <|> expressionP
  wcharP ';'
  cond <- expressionP
  wcharP ';'
  final <- expressionP
  wcharP ')'
  body <- parseBlock <|> statementP
  return $ Exp_For init cond final body

procP :: Parser Char Expression
procP = do
  ws *> stringP "proc "
  func_name <- wss $ (Exp_String <$> wordP)
  args <- do
    wcharP '('
    args <- argsP <|> nop []
    wcharP ')'
    return args
  ws
  stringP "->"
  ws
  rettype <- typeP
  body <- parseBlock <|> statementP
  return $ Exp_Proc func_name args body rettype
    where
      argsP = do
        name <- wordP
        wcharP ':'
        typename <- typeP
        nexts <- (wcharP ',' *> argsP) <|> nop []
        return $ (name, typename):nexts
      commas_to_list :: Expression -> [Expression]
      commas_to_list (Exp_Comma e1 e2) = ((commas_to_list e1) ++ [e2])
      commas_to_list e = [e]

-- commentP :: Parser Char ()
-- commentP = do
  

parseBlock :: Parser Char Expression
parseBlock = Exp_SourceBlock <$> (wcharP '{' *> block)
    where
      block = do
        cur <- statementP
        rest <- block <|> (const [] <$> (wcharP '}'))
        return $ (cur:rest)

statementP :: Parser Char Expression
statementP = controlStructureP <|> ((declarationP <|> returnExpP <|> expressionP) <* (wcharP ';'))

returnExpP :: Parser Char Expression
returnExpP = do
  stringP "return "
  exp <- expressionP
  return $ Exp_Return $ exp

controlStructureP :: Parser Char Expression
controlStructureP = ifP <|> whileP <|> forP

typeP :: Parser Char Type
typeP =
  ((wcharP '*') *> (Type_Pointer <$> typeP)) <|>
  (const Type_I64 <$> stringP "i64") <|> 
  (const Type_I8 <$> stringP "i8") <|> 
  (const Type_Empty <$> stringP "()") <|>
  (Type_Arr <$> (wcharP '[' *> typeP) <*> (wcharP ';' *> (integerP) <* wcharP ']')) 

declarationP :: Parser Char Expression
declarationP = do
  wss $ stringP "let "
  varname <- wordP
  wcharP ':'
  t <- typeP
  rhs <- wcharP '=' *> expressionP
  return $ Exp_Declaration varname t rhs
 
data OperatorLevel =
  BinaryOperatorList [(String, Expression -> Expression -> Expression)] |
  PrefixUnaryOperatorList [(String, Expression -> Expression)] |
  PostfixUnaryOperatorList [(String, Expression -> Expression)] |
  FixedLevel Int

instance Show OperatorLevel where
  show (FixedLevel i) = show i
  show (BinaryOperatorList bs) = show $ map fst bs

expressionP :: Parser Char Expression
expressionP = head operators
    where
      operators_raw = [
        -- lowest precedence
        BinaryOperatorList [((","), Exp_Comma)],
        BinaryOperatorList [(("="), Exp_Assignment)],
        BinaryOperatorList [("||", Exp_Or)],
        BinaryOperatorList [("&&", Exp_And)],
        BinaryOperatorList [("<=", Exp_LessEqual), (">=", Exp_GreaterEqual), ("<", Exp_LessThan), (">", Exp_GreaterThan), ("==", Exp_Equality)],
        BinaryOperatorList [("+", Exp_Plus), ("-", Exp_Minus)],
        BinaryOperatorList [("*", Exp_Mult), ("/", Exp_Div), ("%", Exp_Mod)],
        FixedLevel 1
        -- highest precedence
        ]
      operators :: [Parser Char Expression]
      operators = foldr (\ ops_list new_ops -> let op = operatorLevelP (head new_ops) ops_list in (op:new_ops)) [parseFinal] operators_raw

parseFinal :: Parser Char Expression
parseFinal = numP <|> (Exp_String <$> wordP) <|> parensP <|> stringLiteralP <|> charLiteralP
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
runParser parser input = putStrLn $ case parser.run $ Input input 0 of
                           Right (exp, rest) -> ((print_exp 0 exp) ++ "\nrest:\n" ++ (show rest))
                           Left err -> show err
