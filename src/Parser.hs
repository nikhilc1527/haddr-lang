module Parser where

import Lexer

import Data.Bool
import Data.Data
import Data.List.Split
import qualified Debug.Trace as Trace
import Data.Hashable
import Text.Printf
import qualified Data.HashMap as Map

type TypeID = Integer

typeID_INT = 1
typeID_STRING = 2
typeID_BOOL = 3
typeID_POINTER = 4
typeID_FUNC = 5

type ExprID = Int
data Expression = Expression ExprID [Expression] [TokenType]
  deriving (Eq, Ord, Show)

-- instance Show Expression
--   where
--     show (Expression id exprs toks) = printf "(%d %s)" id $ bool (show exprs) (show $ head toks) $ length toks > 0

expID_INVALID    = -1
expID_IF         = 0
expID_WHILE      = 1
expID_FUNC       = 2
expID_CALL       = 3
expID_PLUS       = 4
expID_MINUS      = 5
expID_MULT       = 6
expID_DIV        = 7
expID_MOD        = 8
expID_DOUBLE_EQ  = 9
expID_LT         = 10
expID_GT         = 11
expID_AND        = 12
expID_OR         = 13
expID_DUMP       = 14
expID_UNARY      = 15
expID_VALUE      = 16
expID_LHS        = 17
expID_ARR_CREAT  = 18
expID_ARR_ASSIGN = 19
expID_ASSIGNMENT = 20
expID_SRCBLOCK   = 21
expID__          = 22

print_exp :: Int -> Expression -> String
print_exp spaces (Expression exprID exprs tokens)
  | expID__ /= 22 = error "Print Expression not exhaustive handling"
  | exprID == expID_IF          = let (cond:e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "IF\n"  ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_WHILE       = let (cond:e1:[]) = exprs in (take spaces $ repeat ' ') ++ "WHILE\n"  ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) e1)
  | exprID == expID_FUNC        = let (body:[]) = exprs  in (take spaces $ repeat ' ') ++ "FUNC\n"  ++ (take (spaces+2) $ repeat ' ') ++ (show $ head $ tokens) ++ "\n" ++ (take (spaces+2) $ repeat ' ') ++ (show $ tail $ tokens) ++ "\n" ++ (print_exp (spaces+2) $ head exprs)
  | exprID == expID_PLUS        = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "PLUS\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_MINUS       = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "MINUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_MULT        = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "MULT\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_DIV         = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "DIV\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_MOD         = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "MOD\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_LT          = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "LT\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_GT          = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "GT\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_DOUBLE_EQ   = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "EQUALITY\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_AND         = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "AND\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_OR          = let (e1:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "OR\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
  | exprID == expID_DUMP        = let (e1:[])    = exprs in (take spaces $ repeat ' ') ++ "DUMP\n" ++ (print_exp (spaces+2) e1)
  | exprID == expID_ARR_CREAT   = let (e1:[])    = exprs in (take spaces $ repeat ' ') ++ "ARR CREAT\n" ++ (print_exp (spaces+2) e1)
  | exprID == expID_ASSIGNMENT  = let (lhs:e2:[]) = exprs in (take spaces $ repeat ' ') ++ "ASSIGNMENT\n" ++ (print_exp (spaces+2) lhs) ++ (print_exp (spaces+2) e2)
  | exprID == expID_ARR_ASSIGN  = let
                                    (ind:after:[]) = exprs
                                    ((TOK_USERDEF name):[]) = tokens
                                  in
                                    (take spaces $ repeat ' ') ++ "ARR ASSIGNMENT\n" ++
                                    (take (spaces+2) $ repeat ' ') ++ "NAME\n" ++ (take (spaces+4) $ repeat ' ') ++ (show name) ++ "\n" ++ 
                                    (take (spaces+2) $ repeat ' ') ++ "INDEX\n" ++ (print_exp (spaces+4) ind) ++ 
                                    (take (spaces+2) $ repeat ' ') ++ "VALUE\n" ++ (print_exp (spaces+4) after)
  | exprID == expID_LHS         = let (t1:[]) = tokens in (take spaces $ repeat ' ') ++ (show t1) ++ "\n"
  | exprID == expID_VALUE =
      if | length exprs > 0 ->
             let
               (t1:[]) = tokens
               (e:[]) = exprs
             in
               (take spaces $ repeat ' ') ++ (show t1) ++ "\n" ++ (take spaces $ repeat ' ') ++ "INDEX\n" ++ (print_exp (spaces+2) e)
         | True -> let (t1:[]) = tokens in (take spaces $ repeat ' ') ++ (show t1) ++ "\n"
     
  | exprID == expID_SRCBLOCK    = (take spaces $ repeat ' ') ++ "SOURCE BLOCK\n" ++ (foldl (++) "" $ map (print_exp (spaces+2)) exprs)
  | exprID == expID_CALL        = (take spaces $ repeat ' ') ++ "FUNC CALL\n" ++ (take (spaces+2) $ repeat ' ') ++ (show $ head tokens) ++ "\n" ++ (foldl (++) "" $ map (print_exp (spaces+2)) exprs)
  | exprID == expID_INVALID     = "\n" ++ (take spaces $ repeat ' ') ++ "--- INVALID EXPR ---\n" ++ "\n"
  | True                        = error "could not identify type of expression while printing"

invalid_parse :: (Expression, [TokenType])
invalid_parse = (Expression expID_INVALID [] [], [])

insideBlock = untilClose [TOK_DO] [TOK_END] 1

-- params:  opening tokens, closing tokens, initial stack amount (default 1), tokens to get
-- returns: list of tokens inside, list of tokens outside (not including closing token)
untilClose :: [TokenType] -> [TokenType] -> Int -> [TokenType] -> ([TokenType], [TokenType])
untilClose opening closing stack tokens
  | null tokens                                 = error "unable to find closing tokens"
  | head tokens `elem` closing && stack == 1 = ([], tail tokens)
  | head tokens `elem` closing               = let (a, b) = untilClose opening closing (stack-1) $ tail $ tokens in ([head tokens] ++ a, b)
  | head tokens `elem` opening               = let (a, b) = untilClose opening closing (stack+1) $ tail $ tokens in ([head tokens] ++ a, b)
  | True                                     = let (a, b) = untilClose opening closing ( stack ) $ tail $ tokens in ([head tokens] ++ a, b)

parseL0 :: [TokenType] -> (Expression, [TokenType]) -- returns maybe a syntax tree of expression or nothing if couldnt parse, and if could parse then the 
parseL0 [] = invalid_parse
parseL0 _
  | expID__ /= 22 = error "Parse Expression not exhaustive handling"
-- dump operator
-- . <expr>
parseL0 (TOK_DUMP:tokens_0)
  | True = ((Expression expID_DUMP [next_val] []), symtab2)
    where
      (next_val, symtab2) = parseL0 tokens_0
-- function declaration
-- func <name> <param...> do <body> end
parseL0 (TOK_FUNC:tokens_0) = ((Expression expID_FUNC [body_parsed] args), rest_tokens)
  where
    funcname    = head tokens_0
    args        = takeWhile (/= TOK_DO) $ tokens_0
    (body_tokens, rest_tokens) = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_END] 1 $ tail $ dropWhile (/= TOK_DO) $ tokens_0
    body_parsed = parseSource body_tokens
-- if statement
-- if <cond> do <body> else <body> end
parseL0 (TOK_IF:tokens_0)
  | cond_1_id == expID_INVALID        = invalid_parse
  | expr_true_2_id == expID_INVALID   = invalid_parse
  | expr_false_3_id == expID_INVALID  = invalid_parse
  | True = (Expression expID_IF [cond_1, expr_true_2, expr_false_3] [], tokens_3)
  where
    (cond_tokens, tokens_1)        = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_DO] 1 tokens_0
    cond_1                         = parseSource $ cond_tokens -- parse the condition, which should be everything inside of 'if' and 'do'
    (Expression cond_1_id cond_1_exprs cond_1_tokens) = cond_1

    (expr_true_tokens, tokens_2)   = untilClose [TOK_IF] [TOK_ELSE] 1 tokens_1
    expr_true_2                         = parseSource $ expr_true_tokens -- parse the expression that happens if the condition is true
    (Expression expr_true_2_id expr_true_2_exprs expr_true_2_tokens) = expr_true_2

    (expr_false_tokens, tokens_3)  = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_END] 1 tokens_2
    expr_false_3                         = parseSource $ expr_false_tokens -- parse the expression that happens if the condition is false
    (Expression expr_false_3_id expr_false_3_exprs expr_false_3_tokens) = expr_false_3
-- while loop
-- while <cond> do <body> end
parseL0 (TOK_WHILE:tokens_0)
  | cond_1_id == expID_INVALID = invalid_parse
  | expr_2_id == expID_INVALID = invalid_parse
  | True = ((Expression expID_WHILE [cond_1, expr_2] []), tokens_2)
  where
    (cond_tokens, tokens_1)        = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_DO] 1 $ tokens_0
    cond_1                         = parseSource $ cond_tokens -- parse the condition, which should be everything inside of the if and the do
    (Expression cond_1_id _ _) = cond_1

    (expr_tokens, tokens_2)        = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_END] 1 tokens_1
    expr_2                         = parseSource $ expr_tokens -- parse the expression that happens if the condition is true
    (Expression expr_2_id _ _) = expr_2
-- assignment
parseL0 ((TOK_USERDEF x):TOK_SQUAREBRACKETOPEN:tokens)
  | TOK_EQUALS `elem` tokens = res
  where
    (toks_inside, tokens2) = untilClose [TOK_SQUAREBRACKETOPEN] [TOK_SQUAREBRACKETCLOSE] 1 tokens
    inside_exp = parseSource toks_inside

    res = case tokens2 of
      (TOK_EQUALS:tokens3) ->
        let (e, tokens4) = parseL0 tokens3
        in ((Expression expID_ARR_ASSIGN [inside_exp, e] [(TOK_USERDEF x)]), tokens4)
      tokens3 -> ((Expression expID_VALUE [inside_exp] [(TOK_USERDEF x)]), tokens3)
parseL0 ((TOK_USERDEF x):tokens)
  | TOK_EQUALS `elem` tokens = ((Expression expID_ASSIGNMENT [(Expression expID_LHS [] [(TOK_USERDEF x)]), afterEqExp] []), tokens_1)
  -- | TOK_EQUALS `elem` tokens && lhs_all_userdef = ((Expression expID_ASSIGNMENT [(Expression expID_LHS [] beforeEq), afterEqExp] []), tokens_1)
  -- | TOK_EQUALS `elem` tokens                    = error ("not all userdefs before assignment operator: " ++ (show tokens))
  where
    beforeEq = [TOK_USERDEF x] ++ (takeWhile (/= TOK_EQUALS) $ tokens)
    lhs_all_userdef = foldl (&&) True (map (\x ->
                                              case x of
                                                TOK_USERDEF a -> True
                                                _ -> False) beforeEq)
    afterEq  = tail $ dropWhile (/= TOK_EQUALS) tokens
    (afterEqExp, tokens_1) = parseL0 afterEq
-- function call
parseL0 ((TOK_USERDEF x):TOK_PARENOPEN:tokens) = ((Expression expID_CALL args [TOK_USERDEF x]), rest)
  where
    (arg_tokens,rest) = untilClose [TOK_PARENOPEN] [TOK_PARENCLOSE] 1 tokens
    args = map parseSource $ splitWhen (== TOK_COMMA) arg_tokens
parseL0 ((TOK_SQUAREBRACKETOPEN):tokens) = ((Expression expID_ARR_CREAT [inside_exp] []), rest)
  where
    (inside, rest) = untilClose [TOK_SQUAREBRACKETOPEN] [TOK_SQUAREBRACKETCLOSE] 1 tokens
    inside_exp = parseSource inside
-- indexing
-- arr[<expr>]
-- parseL0 ((TOK_USERDEF x):TOK_SQUAREBRACKETOPEN:tokens) = ((Expression expID_INDEX args [TOK_USERDEF x]), rest)
--   where
--     (arg_tokens,rest) = untilClose [TOK_SQUAREBRACKETOPEN] [TOK_SQUAREBRACKETCLOSE] 1 tokens
--     args = map parseSource $ splitWhen (== TOK_COMMA) arg_tokens
parseL0 tokens = parseL1 $ tokens

-- takes [(operator token, operator expression)] 
operatorParser :: [(TokenType, ExprID)] -> ([TokenType] -> (Expression, [TokenType])) -> [TokenType] -> (Expression, [TokenType])
operatorParser operators next_parser [] = invalid_parse
operatorParser operators next_parser tokens_0
  | np_1_id == expID_INVALID = Trace.trace "getting invalid parse from next parse" $ invalid_parse
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
        resultExp = Expression exprID [prev_exp, np_2] []
        rest_ops = get_all_ops resultExp tokens_2
      in
        bool rest_ops (prev_exp, rest_tokens) (null operatorMatch)

parseL1 = operatorParser [(TOK_AND, expID_AND), (TOK_OR, expID_OR)] parseL2

parseL2 = operatorParser [(TOK_LT, expID_LT), (TOK_GT, expID_GT), (TOK_DOUBLE_EQUALS, expID_DOUBLE_EQ)] parseL3

parseL3 = operatorParser [(TOK_PLUS, expID_PLUS), (TOK_MINUS, expID_MINUS)] parseL4

parseL4 = operatorParser [(TOK_MULT, expID_MULT), (TOK_DIV, expID_DIV), (TOK_MOD, expID_MOD)] parseL5

parseL5 = parseFinal

parseFinal :: [TokenType] -> (Expression, [TokenType])
parseFinal [] = invalid_parse
parseFinal (TOK_LITERALNUM s:tokens_0)    = (Expression expID_VALUE [] [TOK_LITERALNUM s], tokens_0)
parseFinal (TOK_LITERALSTRING s:tokens_0) = (Expression expID_VALUE [] [TOK_LITERALSTRING s], tokens_0)
-- ((Expression expID_VALUE [inside_exp] [(TOK_USERDEF x)]), tokens3)
parseFinal ((TOK_USERDEF s):TOK_SQUAREBRACKETOPEN:tokens_0) = ((Expression expID_VALUE [inside_exp] [(TOK_USERDEF s)]), tokens2)
  where
    (toks_inside, tokens2) = untilClose [TOK_SQUAREBRACKETOPEN] [TOK_SQUAREBRACKETCLOSE] 1 tokens_0
    inside_exp = parseSource toks_inside
parseFinal (TOK_USERDEF s:tokens_0) = (Expression expID_VALUE [] [TOK_USERDEF s], rest)
  where
    getops :: [TokenType] -> ([Expression], [TokenType])
    getops [] = ([], [])
    getops tokens
      | expID == expID_INVALID = ([], tokens)
      | True                   = (next_parse:next_ops, rest_toks_2)
       where
        (next_parse, rest_toks) = parseFinal tokens
        (Expression expID _ _)  = next_parse
        (next_ops, rest_toks_2) = getops rest_toks
    (exprs, rest) = getops tokens_0
    -- value = Expression expID_VALUE [] toks
    -- funccall = Expression expID_CALL [] toks
    -- ret = bool funccall value (length toks == 1)
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
  | True = Expression expID_SRCBLOCK (first_expr:exprs) []
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
    (inside, rest2) = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_END] 1 rest
    (statement, outside) = nextStatement rest2
  in ([TOK_IF] ++ inside ++ [TOK_END] ++ statement, outside)
nextStatement (TOK_WHILE:rest)  = 
  let 
    (inside, rest2) = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_END] 1 rest
    (statement, outside) = nextStatement rest2
  in ([TOK_WHILE] ++ inside ++ [TOK_END] ++ statement, outside)
nextStatement (TOK_FUNC:rest)  = 
  let 
    (inside, rest2) = untilClose [TOK_IF, TOK_WHILE, TOK_FUNC] [TOK_END] 1 rest
    (statement, outside) = nextStatement rest2
  in ([TOK_FUNC] ++ inside ++ [TOK_END] ++ statement, outside)
nextStatement (x:rest)  = 
  let 
    (statement, outside) = nextStatement rest
  in ([x] ++ statement, outside)
