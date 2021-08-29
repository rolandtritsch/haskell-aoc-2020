-- |
-- Problem: <https://adventofcode.com/2020/day/18>
--
-- Solution:
--
-- General - We kinda need to solve two problems here: We need to parse
-- the expressions and we need to evaluate the expressions.
--
-- Looking at the expressions you can see an (binary) abstract syntax tree (AST)
-- where every node is an expression. The (two) children are either a value
-- or an expression again.
--
-- To build the AST we first convert the expression string from infix notation
-- to postfix notation (RPN) (with the shunting-yard algorithm (SYA)).
--
-- Building the AST from RPN is trivial (Note: We make this easy by reversing
-- the RPN (12+ -> +21)).
--
-- To evaluate the AST we just have to (recursively) walk the tree.
--
-- Part 1 - Evaluate all expressions and sum up the results.
--
-- Part 2 - ???
module Day18 where

import Data.Char (digitToInt)
import Util (inputRaw)
import Prelude

-- | All possible expressions.
data Expression
  = Add Expression Expression
  | Mul Expression Expression
  | Val Int
  | Nil
  deriving (Eq, Show)

-- | Push a token onto the stack (for SYA).
pushS :: Char -> String -> String
pushS token stack = [token] ++ stack

-- | Pop a token onto the stack (for SYA).
popS :: String -> (Char, String)
popS stack = (head stack, tail stack)

-- | Push a token into the queue (for SYA).
pushQ :: Char -> String -> String
pushQ token queue = queue ++ [token]

-- | Convert expression string into postfix (RPN) string
-- (by running the shunting-yard algorithm (SYA)).
toPostfix :: String -> String
toPostfix infix' = toPostfixToken infix' [] []

-- | Implement SYA.
toPostfixToken :: String -> String -> String -> String
toPostfixToken (token : tokens) output operators
  | token == ' ' = toPostfixToken tokens output operators
  | token == '+' = toPostfixToken tokens output (pushS token operators)
  | token == '*' = toPostfixToken tokens output (pushS token operators)
  | token == '(' = toPostfixToken tokens output (pushS token operators)
  | token == ')' = toPostfixToken tokens output' operators'
  | otherwise = toPostfixToken tokens (pushQ token output) operators
  where
    (output', operators') = toPostfixToken' output operators
toPostfixToken [] output [] = output
toPostfixToken [] output operators = toPostfixToken [] (pushQ o output) os
  where
    (o, os) = popS operators

-- | Implement SYA.
toPostfixToken' :: String -> String -> (String, String)
toPostfixToken' output'' operators''
  | head operators'' == '(' = (output'', snd $ popS operators'')
  | otherwise = toPostfixToken' (pushQ o output'') os
  where
    (o, os) = popS operators''

-- | Turn a postfix/RPN string into an expression AST.
toExpression :: String -> Expression
toExpression postfix = fst $ go (reverse postfix)
  where
    go (token : tokens)
      | token == '+' = (Add left right, tokens'')
      | token == '*' = (Mul left right, tokens'')
      | otherwise = (Val (digitToInt token), tokens)
      where
        (left, tokens') = go tokens
        (right, tokens'') = go tokens'
    go [] = (Nil, [])

-- | Turn a postfix/RPN string into an expression AST (part1).
toExpression' :: String -> Expression
toExpression' postfix = fst $ go (reverse postfix)
  where
    go (token : tokens)
      | token == '+' = (Add left right, tokens'')
      | token == '*' = (Mul left right, tokens'')
      | otherwise = (Val (digitToInt token), tokens)
      where
        (left, tokens') = go tokens
        (right, tokens'') = go tokens'
    go [] = (Nil, [])

-- | Reverse a/the give postfix/RPN string (to make the operators
-- asscociate to the left). Note: For this to work we need to flip
-- the parenthis.
reverse' :: String -> String
reverse' ('(' : tokens) = reverse' tokens ++ [')']
reverse' (')' : tokens) = reverse' tokens ++ ['(']
reverse' (token : tokens) = reverse' tokens ++ [token]
reverse' [] = []

-- | Read the input file.
input :: String -> [Expression]
input filename = map processLine $ lines $ inputRaw filename
  where
    processLine l = toExpression $ toPostfix $ reverse' l

-- | Read the input file.
input' :: String -> [Expression]
input' filename = map processLine $ lines $ inputRaw filename
  where
    processLine l = toExpression' $ toPostfix $ reverse' l

-- | (Recursively) Evaluate an expression.
eval :: Expression -> Int
eval (Add e e') = eval e + eval e'
eval (Mul e e') = eval e * eval e'
eval (Val n) = n
eval Nil = 0

-- | (Recursively) Determine (and return) the size (number of values)
-- of the given expression.
size :: Expression -> Int
size (Add e e') = size e + size e'
size (Mul e e') = size e + size e'
size (Val _) = 1
size Nil = 0

part1 :: [Expression] -> Int
part1 expressions = sum $ map eval expressions

part2 :: [Expression] -> Int
part2 expressions = sum $ map eval expressions
