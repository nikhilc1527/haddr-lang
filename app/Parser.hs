module Parser where

import Lexer

import Data.Bool
import Data.Data
import qualified Debug.Trace as Trace
import Data.Hashable

data Expression =
  EXP_IF Expression Expression Expression              | -- if expression then expression else expression
  EXP_WHILE Expression Expression                      | -- while expression expression
  EXP_BINARY Expression Expression                     | -- expression for binary operators (+, -, mod, etc)
  EXP_PLUS Expression Expression                       | -- expression for binary operators (+, -, mod, etc)
  EXP_MINUS Expression Expression                      | -- expression for binary operators (+, -, mod, etc)
  EXP_MULT Expression Expression                       | -- expression for binary operators (+, -, mod, etc)
  EXP_DIV Expression Expression                        | -- expression for binary operators (+, -, mod, etc)
  EXP_UNARY Expression                                 | -- expression for unary operators (none as of now)
  EXP_VALUE TokenType                                  | -- a single value, could be either a literal value like an int or string or could be a variable name
  EXP_LHS [TokenType]                                  | -- left hand side of assignment, could be either a single variable name or a list of variable names which signifies a function definition
  EXP_ASSIGNMENT Expression Expression                 | -- the assignment is an expression on one side (could be either just a variable name or could be a function type)
  EXP_SRCBLOCK [Expression]                            | -- a list of expressions that are part of one bigger block of code
  EXP_INVALID
  deriving (Eq, Show, Ord)

print_exp :: Int -> Expression -> String
print_exp spaces (EXP_IF cond e1 e2)     = (take spaces $ repeat ' ') ++ "IF\n"  ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_WHILE cond e1)     = (take spaces $ repeat ' ') ++ "WHILE\n"  ++ (print_exp (spaces+2) cond) ++ (print_exp (spaces+2) e1)
print_exp spaces (EXP_PLUS e1 e2)        = (take spaces $ repeat ' ') ++ "PLUS\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_MINUS e1 e2)       = (take spaces $ repeat ' ') ++ "MINUS\n" ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_MULT e1 e2)        = (take spaces $ repeat ' ') ++ "MULT\n"  ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_DIV e1 e2)         = (take spaces $ repeat ' ') ++ "DIV\n"   ++ (print_exp (spaces+2) e1) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_ASSIGNMENT lhs e2) = (take spaces $ repeat ' ') ++ "ASSIGNMENT\n" ++ (print_exp (spaces+2) lhs) ++ (print_exp (spaces+2) e2)
print_exp spaces (EXP_LHS t1)            = (take spaces $ repeat ' ') ++ (show t1) ++ "\n"
print_exp spaces (EXP_VALUE t1)          = (take spaces $ repeat ' ') ++ (show t1) ++ "\n"
print_exp spaces (EXP_SRCBLOCK blk)      = (take spaces $ repeat ' ') ++ "SOURCE BLOCK\n" ++ (foldl (++) "" $ map (print_exp (spaces+2)) blk)
print_exp spaces (EXP_INVALID)           = (take spaces $ repeat ' ') ++ "INVALID EXPR" ++ "\n"
print_exp spaces _                       = error "could not identify type of expression while printing"

invalid_parse = (EXP_INVALID, [])

parseL0 :: [TokenType] -> (Expression, [TokenType]) -- returns maybe a syntax tree of expression or nothing if couldnt parse, and if could parse then the 
parseL0 [] = invalid_parse
parseL0 (TOK_IF:tokens_0)
  | cond_1 == EXP_INVALID = invalid_parse
  | tokens_1 == [] = invalid_parse
  | expr_true_2 == EXP_INVALID = invalid_parse
  | tokens_2 == [] = invalid_parse
  | expr_false_3 == EXP_INVALID = invalid_parse
  | True = case tokens_1 of
             (TOK_DO:_) ->
               case tokens_2 of
                 (TOK_ELSE:_) ->
                   case tokens_3 of
                     (TOK_END:rest) -> ((EXP_IF cond_1 expr_true_2 expr_false_3), rest) -- case where all of them are good, no invalid parsing
                     _ -> invalid_parse
                 _ -> invalid_parse
             _  -> invalid_parse
  where
    (cond_1, tokens_1)       = parseL3 $ tokens_0 -- parse the condition, which should be everything inside of the if and the do
    (expr_true_2, tokens_2)  = parseL3 $ tail $ tokens_1 -- parse the expression that happens if the condition is true
    (expr_false_3, tokens_3) = parseL3 $ tail $ tokens_2 -- parse the expression that happens if the condition is false
parseL0 (TOK_WHILE:tokens_0)
  | cond_1 == EXP_INVALID = invalid_parse
  | tokens_1 == [] = invalid_parse
  | expr_2 == EXP_INVALID = invalid_parse
  | tokens_2 == [] = invalid_parse
  | True = case tokens_1 of
             (TOK_DO:_) ->
               case tokens_2 of
                 (TOK_END:rest) -> ((EXP_WHILE cond_1 expr_2), rest) -- case where all of them are good, no invalid parsing
                 _ -> invalid_parse
             _  -> invalid_parse
  where
    (cond_1, tokens_1) = parseL3 $ tokens_0 -- parse the condition, which should be everything inside of the if and the do
    (expr_2, tokens_2) = parseL3 $ tail $ tokens_1 -- parse the expression that happens if the condition is true
parseL0 tokens
  | TOK_EQUALS `elem` tokens && lhs_all_userdef = ((EXP_ASSIGNMENT (EXP_LHS beforeEq) afterEqExp), tokens_1)
  | TOK_EQUALS `elem` tokens                    = error ("not all userdefs before assignment operator: " ++ (show tokens))
  where
    beforeEq = takeWhile (/= TOK_EQUALS) tokens
    lhs_all_userdef = foldl (&&) True (map (\x ->
                                              case x of
                                                TOK_USERDEF a -> True
                                                _ -> False) beforeEq)
    afterEq  = tail $ dropWhile (/= TOK_EQUALS) tokens
    (afterEqExp, tokens_1) = parseL0 afterEq
parseL0 tokens = parseL1 tokens

parseL1 :: [TokenType] -> (Expression, [TokenType])
parseL1 [] = invalid_parse
parseL1 tokens_0
  | l2_1 == EXP_INVALID = invalid_parse
  | null tokens_1       = (l2_1, tokens_1)
  | True                = get_all_l1s l2_1 tokens_1
  where
    (l2_1, tokens_1) = parseL2 tokens_0

    get_all_l1s prev_exp [] = (prev_exp, [])
    get_all_l1s prev_exp rest_tokens =
      let
        (l2_2, tokens_2) = parseL2 $ tail rest_tokens
      in
        case (head rest_tokens) of
          TOK_PLUS  -> get_all_l1s (EXP_PLUS prev_exp l2_2) tokens_2
          TOK_MINUS -> get_all_l1s (EXP_MINUS prev_exp l2_2) tokens_2
          _ -> (prev_exp, rest_tokens)

parseL2 :: [TokenType] -> (Expression, [TokenType])
parseL2 [] = invalid_parse
parseL2 tokens_0
  | l3_1 == EXP_INVALID = invalid_parse
  | null tokens_1       = (l3_1, tokens_1)
  | True                = get_all_l2s l3_1 tokens_1
  where
    (l3_1, tokens_1) = parseL3 tokens_0
    
    get_all_l2s prev_exp [] = (prev_exp, [])
    get_all_l2s prev_exp rest_tokens =
      let
        (l3_2, tokens_2) = parseL3 $ tail rest_tokens
      in
        case (head rest_tokens) of
          TOK_MULT -> get_all_l2s (EXP_MULT prev_exp l3_2) tokens_2
          TOK_DIV  -> get_all_l2s (EXP_DIV prev_exp l3_2) tokens_2
          _ -> (prev_exp, rest_tokens)

parseL3 :: [TokenType] -> (Expression, [TokenType])
parseL3 [] = invalid_parse
parseL3 (TOK_USERDEF s:tokens_0)       = (EXP_VALUE $ TOK_USERDEF s, tokens_0)
parseL3 (TOK_LITERALNUM s:tokens_0)    = (EXP_VALUE $ TOK_LITERALNUM s, tokens_0)
parseL3 (TOK_LITERALSTRING s:tokens_0) = (EXP_VALUE $ TOK_LITERALSTRING s, tokens_0)

parseL3 tokens_0
  | head tokens_0 /= TOK_PARENOPEN   = invalid_parse
  | insideParensParsed == EXP_INVALID              = invalid_parse
  | null tokens_1                    = invalid_parse
  | True = (insideParensParsed, tokens_1)
  where
    untilClose stack tokens
      | head tokens == TOK_PARENCLOSE && stack == 1 = ([], tail tokens)
      | head tokens == TOK_PARENCLOSE = let (a, b) = untilClose (stack-1) $ tail $ tokens in ([head tokens] ++ a, b)
      | head tokens == TOK_PARENOPEN  = let (a, b) = untilClose (stack+1) $ tail $ tokens in ([head tokens] ++ a, b)
      | True                          = let (a, b) = untilClose ( stack ) $ tail $ tokens in ([head tokens] ++ a, b)
    (insideParens, tokens_1) = untilClose 1 $ tail tokens_0
    insideParensParsed = parseSource insideParens

-- input: full lexed source of the input program
-- output: either EXP_SRCBLOCK for successfull parsing or EXP_INVALID for invalid parsing
parseSource :: [TokenType] -> Expression 
parseSource [] = EXP_SRCBLOCK []
parseSource tokens
  | not $ null rest_first_tokens = error ("could not parse expression " ++ (show first_stmt))
  | True = EXP_SRCBLOCK (first_expr:exprs)
  where
    (first_stmt, rest_tokens) = nextStatement tokens 0 0
    (first_expr, rest_first_tokens) = parseL0 first_stmt
    (EXP_SRCBLOCK exprs) = parseSource rest_tokens

-- block is everything surrounded by curly brackets. the value of a block can either be nothing, or some value that is returned
parseBlock :: [[TokenType]] -> [Expression]
parseBlock = undefined

getStatements :: [TokenType] -> [[TokenType]]
getStatements [] = []
getStatements tokens = a:(getStatements b)
  where
    (a, b) = nextStatement tokens 0 0

-- get the next statement delimited by a semicolon
-- parameters: tokens, current paren stack, current brackets stack, (first statement, rest of tokens)
nextStatement :: [TokenType] -> Int -> Int -> ([TokenType],[TokenType])
nextStatement [] p_stack b_stack = ([],[])
nextStatement (TOK_SEMICOLON:rest) 0 0 = ([],rest)
nextStatement (TOK_PARENOPEN:rest) a b = (([TOK_PARENOPEN] ++ first), second)
  where
    (first, second) = nextStatement rest (a+1) b
nextStatement (TOK_PARENCLOSE:rest) a b = (([TOK_PARENCLOSE] ++ first), second)
  where
    (first, second) = nextStatement rest (a-1) b
nextStatement (TOK_CURLYBRACKETOPEN:rest) a b = (([TOK_CURLYBRACKETOPEN] ++ first), second)
  where
    (first, second) = nextStatement rest a (b+1)
nextStatement (TOK_CURLYBRACKETCLOSE:rest) a b = (([TOK_CURLYBRACKETCLOSE] ++ first), second)
  where
    (first, second) = nextStatement rest a (b-1)
nextStatement (cur:rest) a b = (([cur] ++ first), second)
  where
    (first, second) = nextStatement rest a b
