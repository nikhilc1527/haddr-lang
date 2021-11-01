module Parser where

import Lexer

import Data.Bool
import Data.Data
import qualified Debug.Trace as Trace
import Data.Hashable

type ExprID = Int
data Expression = Expression ExprID [Expression] [TokenType]
  deriving (Eq, Show, Ord)

expID_INVALID = -1
expID_IF = 0
expID_WHILE = 1
expID_BINARY = 2
expID_PLUS = 3
expID_MINUS = 4
expID_MULT = 5
expID_DIV = 6
expID_MOD = 7
expID_LT = 8
expID_GT = 9
expID_AND = 10
expID_OR = 11
expID_UNARY = 12
expID_VALUE = 13
expID_LHS = 14
expID_ASSIGNMENT = 15
expID_SRCBLOCK = 16

print_exp :: Int -> Expression -> String
print_exp spaces (Expression exprID exprs tokens)
  | exprID == expID_IF          = let (cond:e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "IF\n"  ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_WHILE       = let (cond:e1:[]) = exprs in (take spaces $ repeat ' ') ++ "WHILE\n"  ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) e1)
  | exprID == expID_PLUS        = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "PLUS\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_MINUS       = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "MINUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_MULT        = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "MULT\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_DIV         = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "DIV\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_MOD         = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "MOD\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_LT          = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "LT\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_GT          = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "GT\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_ASSIGNMENT  = let (lhs:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "ASSIGNMENT\n" ++ (print_exp (spaces+2) lhs) ++ (print_exp (spaces+2) e2)
  | exprID == expID_LHS         = let (t1:[]) = tokens in (take spaces $ repeat ' ') ++ (show t1) ++ "\n"
  | exprID == expID_VALUE       = let (t1:[]) = tokens in (take spaces $ repeat ' ') ++ (show t1) ++ "\n"
  | exprID == expID_SRCBLOCK    = let (blk:[]) = exprs in (take spaces $ repeat ' ') ++ "SOURCE BLOCK\n" ++ (foldl (++) "" $ map (print_exp (spaces+2)) exprs)
  | exprID == expID_INVALID     = "\n" ++ (take spaces $ repeat ' ') ++ "--- INVALID EXPR ---\n" ++ "\n"
  | True                        = error "could not identify type of expression while printing"

invalid_parse :: (Expression, [TokenType])
invalid_parse = (Expression expID_INVALID [] [], [])

untilClose :: [TokenType] -> [TokenType] -> Int -> [TokenType] -> ([TokenType], [TokenType])
untilClose opening closing stack tokens
  | null tokens                                 = error "unable to find closing tokens"
  | head tokens `elem` closing && stack == 1 = ([], tail tokens)
  | head tokens `elem` closing               = let (a, b) = untilClose opening closing (stack-1) $ tail $ tokens in ([head tokens] ++ a, b)
  | head tokens `elem` opening               = let (a, b) = untilClose opening closing (stack+1) $ tail $ tokens in ([head tokens] ++ a, b)
  | True                                     = let (a, b) = untilClose opening closing ( stack ) $ tail $ tokens in ([head tokens] ++ a, b)

parseL0 :: [TokenType] -> (Expression, [TokenType]) -- returns maybe a syntax tree of expression or nothing if couldnt parse, and if could parse then the 
parseL0 [] = invalid_parse
parseL0 (TOK_IF:tokens_0)
  | cond_1_id == expID_INVALID        = invalid_parse
  | expr_true_2_id == expID_INVALID   = invalid_parse
  | expr_false_3_id == expID_INVALID  = invalid_parse
  | True = (Expression expID_IF [cond_1, expr_true_2, expr_false_3] [], tokens_3)
  where
    (cond_tokens, tokens_1)        = untilClose [TOK_IF, TOK_WHILE] [TOK_DO] 1 tokens_0
    cond_1                         = parseSource $ cond_tokens -- parse the condition, which should be everything inside of 'if' and 'do'
    (Expression cond_1_id cond_1_exprs cond_1_tokens) = cond_1

    (expr_true_tokens, tokens_2)   = untilClose [TOK_IF] [TOK_ELSE] 1 tokens_1
    expr_true_2                         = parseSource $ expr_true_tokens -- parse the expression that happens if the condition is true
    (Expression expr_true_2_id expr_true_2_exprs expr_true_2_tokens) = expr_true_2

    (expr_false_tokens, tokens_3)  = untilClose [TOK_IF, TOK_WHILE] [TOK_END] 1 tokens_2
    expr_false_3                         = parseSource $ expr_false_tokens -- parse the expression that happens if the condition is false
    (Expression expr_false_3_id expr_false_3_exprs expr_false_3_tokens) = expr_false_3
    
parseL0 (TOK_WHILE:tokens_0)
  | cond_1_id == expID_INVALID = invalid_parse
  | expr_2_id == expID_INVALID = invalid_parse
  | True = ((Expression expID_WHILE [cond_1, expr_2] []), tokens_2)
  where
    (cond_tokens, tokens_1)        = untilClose [TOK_IF, TOK_WHILE] [TOK_DO] 1 $ Trace.trace ("while loop got tokens: " ++ (show tokens_0)) $ tokens_0
    cond_1                         = parseSource $ Trace.trace ("rest tokens: " ++ (show tokens_1)) $ cond_tokens -- parse the condition, which should be everything inside of the if and the do
    (Expression cond_1_id _ _) = cond_1

    (expr_tokens, tokens_2)        = untilClose [TOK_IF, TOK_WHILE] [TOK_END] 1 tokens_1
    expr_2                         = parseSource $ Trace.trace ("while expression tokens: " ++ (show expr_tokens)) $ expr_tokens -- parse the expression that happens if the condition is true
    (Expression expr_2_id _ _) = expr_2
parseL0 ((TOK_USERDEF x):tokens)
  | TOK_EQUALS `elem` tokens && lhs_all_userdef = ((Expression expID_ASSIGNMENT [(Expression expID_LHS [] beforeEq), afterEqExp] []), tokens_1)
  | TOK_EQUALS `elem` tokens                    = error ("not all userdefs before assignment operator: " ++ (show tokens))
  where
    beforeEq = [TOK_USERDEF x] ++ (takeWhile (/= TOK_EQUALS) $ tokens)
    lhs_all_userdef = foldl (&&) True (map (\x ->
                                              case x of
                                                TOK_USERDEF a -> True
                                                _ -> False) beforeEq)
    afterEq  = tail $ dropWhile (/= TOK_EQUALS) tokens
    (afterEqExp, tokens_1) = parseL0 afterEq
parseL0 tokens = parseL1 $ tokens

-- takes [(operator token, operator expression)] 
operatorParser :: [(TokenType, ExprID)] -> ([TokenType] -> (Expression, [TokenType])) -> [TokenType] -> (Expression, [TokenType])
operatorParser operators next_parser [] = invalid_parse
operatorParser operators next_parser tokens_0
  | np_1_id == expID_INVALID = invalid_parse
  | null tokens_1       = (np_1, tokens_1)
  | True                = get_all_ops np_1 tokens_1
  where
    -- np == next_parser
    (np_1, tokens_1) = next_parser tokens_0
    (Expression np_1_id _ _) = np_1

    get_all_ops :: Expression -> [TokenType] -> (Expression, [TokenType])
    get_all_ops prev_exp [] = (prev_exp, [])
    get_all_ops prev_exp rest_tokens =
      let
        (np_2, tokens_2) = next_parser $ tail rest_tokens
        operatorMatch = filter (\ (token, exprID) -> head rest_tokens == token) operators
        exprID = let (tokentype, expr) = head operatorMatch in expr
      in
        bool ((Expression exprID [prev_exp, np_2] []), tokens_2) (prev_exp, rest_tokens) (null operatorMatch)

parseL1 = operatorParser [(TOK_LT, expID_LT), (TOK_GT, expID_GT)] parseL2

parseL2 = operatorParser [(TOK_AND, expID_AND), (TOK_OR, expID_OR)] parseL3

parseL3 = operatorParser [(TOK_PLUS, expID_PLUS), (TOK_MINUS, expID_MINUS)] parseL4

parseL4 = operatorParser [(TOK_MULT, expID_MULT), (TOK_DIV, expID_DIV), (TOK_MOD, expID_MOD)] parseL5

parseL5 = parseFinal

parseFinal :: [TokenType] -> (Expression, [TokenType])
parseFinal [] = invalid_parse
parseFinal (TOK_USERDEF s:tokens_0)       = (Expression expID_VALUE [] [TOK_USERDEF s], tokens_0)
parseFinal (TOK_LITERALNUM s:tokens_0)    = (Expression expID_VALUE [] [TOK_LITERALNUM s], tokens_0)
parseFinal (TOK_LITERALSTRING s:tokens_0) = (Expression expID_VALUE [] [TOK_LITERALSTRING s], tokens_0)

parseFinal tokens_0
  | head tokens_0 /= TOK_PARENOPEN    = invalid_parse
  | insideParensParsed_id == expID_INVALID = invalid_parse
  | True = (insideParensParsed, tokens_1)
  where
    (insideParens, tokens_1) = untilClose [TOK_PARENOPEN] [TOK_PARENCLOSE] 1 $ tail tokens_0
    insideParensParsed = parseSource $ insideParens
    (Expression insideParensParsed_id _ _) = insideParensParsed

-- input: full lexed source of the input program
-- output: either expID_SRCBLOCK for successfull parsing or expID_INVALID for invalid parsing
parseSource :: [TokenType] -> Expression 
parseSource [] = Expression expID_SRCBLOCK [] []
parseSource tokens
  | not $ null rest_first_tokens = error ("could not parse source " ++ (show first_stmt))
  | True = -- Trace.trace ("parsing source block of: " ++ show tokens) $ 
    Expression expID_SRCBLOCK (first_expr:exprs) []
  where
    (first_stmt, rest_tokens) = nextStatement tokens
    (first_expr, rest_first_tokens) = parseL0 (first_stmt)
    (Expression expID_SRCBLOCK exprs _) = parseSource rest_tokens

getStatements :: [TokenType] -> [[TokenType]]
getStatements [] = []
getStatements tokens = a:(getStatements b)
  where
    (a, b) = nextStatement tokens

-- get the next statement delimited by a semicolon
-- parameters: tokens, current paren stack, current brackets stack, (first statement, rest of tokens)
-- nextStatement :: [TokenType] -> Int -> Int -> ([TokenType],[TokenType])
nextStatement :: [TokenType] -> ([TokenType],[TokenType])
nextStatement []                    = ([],[])
nextStatement (TOK_SEMICOLON:rest)  = ([],rest)
nextStatement (TOK_PARENOPEN:rest)  = 
  let 
    (inside, rest2) = untilClose [TOK_PARENOPEN] [TOK_PARENCLOSE] 1 rest
    (statement, outside) = nextStatement rest2
  in ([TOK_PARENOPEN] ++ inside ++ [TOK_PARENCLOSE] ++ statement, outside)
nextStatement (TOK_IF:rest)  = 
  let 
    (inside, rest2) = untilClose [TOK_IF, TOK_WHILE] [TOK_END] 1 rest
    (statement, outside) = nextStatement rest2
  in ([TOK_IF] ++ inside ++ [TOK_END] ++ statement, outside)
nextStatement (TOK_WHILE:rest)  = 
  let 
    (inside, rest2) = untilClose [TOK_IF, TOK_WHILE] [TOK_END] 1 rest
    (statement, outside) = nextStatement rest2
  in ([TOK_WHILE] ++ inside ++ [TOK_END] ++ statement, outside)
nextStatement (x:rest)  = 
  let 
    (statement, outside) = nextStatement rest
  in ([x] ++ statement, outside)



-- nextStatement (TOK_PARENOPEN:rest) a b = (([TOK_PARENOPEN] ++ first), second)
--   where
--     (first, second) = nextStatement rest (a+1) b
-- nextStatement (TOK_PARENCLOSE:rest) a b = (([TOK_PARENCLOSE] ++ first), second)
--   where
--     (first, second) = nextStatement rest (a-1) b
-- nextStatement (TOK_CURLYBRACKETOPEN:rest) a b = (([TOK_CURLYBRACKETOPEN] ++ first), second)
--   where
--     (first, second) = nextStatement rest a (b+1)
-- nextStatement (TOK_CURLYBRACKETCLOSE:rest) a b = (([TOK_CURLYBRACKETCLOSE] ++ first), second)
--   where
--     (first, second) = nextStatement rest a (b-1)
-- nextStatement (cur:rest) a b = (([cur] ++ first), second)
--   where
--     (first, second) = nextStatement rest a b
