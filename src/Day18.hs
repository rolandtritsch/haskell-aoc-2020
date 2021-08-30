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
-- Part 2 - Now we need to implement precedence (by peeking into the
-- operators stack).
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

-- | Read the input file.
input :: String -> [String]
input filename = lines $ inputRaw filename



-- | Convert infix string into postfix (RPN) string
-- (by running the shunting-yard algorithm (SYA)).
toPostfix :: String -> String
toPostfix infix' = toPostfixToken infix' [] []

-- | Implement SYA (with no prededence; eval left to right)
toPostfixToken :: String -> String -> String -> String
toPostfixToken (' ':tokens) output operators = toPostfixToken tokens output operators
toPostfixToken ('+':tokens) output operators = toPostfixToken tokens output ('+':operators)
toPostfixToken ('*':tokens) output operators = toPostfixToken tokens output ('*':operators)
toPostfixToken ('(':tokens) output operators = toPostfixToken tokens output ('(':operators)
toPostfixToken (')':tokens) output operators = toPostfixToken tokens output' operators'
  where
    (output', operators') = toPostfixToken' output operators
toPostfixToken (token:tokens) output operators = toPostfixToken tokens (output++[token]) operators
toPostfixToken [] output [] = output
toPostfixToken [] output (o:operators) = toPostfixToken [] (output++[o]) operators

-- | Implement SYA.
toPostfixToken' :: String -> String -> (String, String)
toPostfixToken' output ('(':operators) = (output, operators)
toPostfixToken' output (o:operators) = toPostfixToken' (output++[o]) operators
toPostfixToken' _ [] = error "toPostfixToken': Unexpected pattern match"

-- | Turn a postfix/RPN string into an expression AST.
toExpression :: String -> Expression
toExpression postfix = fst $ go (reverse postfix)
  where
    go (token:tokens)
      | token == '+' = (Add left right, tokens'')
      | token == '*' = (Mul left right, tokens'')
      | otherwise = (Val (digitToInt token), tokens)
      where
        (left, tokens') = go tokens
        (right, tokens'') = go tokens'
    go [] = (Nil, [])

-- | Reverse a/the give expression string (to make the operators
-- asscociate to the left). Note: For this to work we need to flip
-- the parenthis.
reverse' :: String -> String
reverse' ('(' : tokens) = reverse' tokens ++ [')']
reverse' (')' : tokens) = reverse' tokens ++ ['(']
reverse' (token : tokens) = reverse' tokens ++ [token]
reverse' [] = []

-- | (Recursively) Evaluate an expression.
eval :: Expression -> Int
eval (Add e e') = eval e + eval e'
eval (Mul e e') = eval e * eval e'
eval (Val n) = n
eval Nil = 0

-- | Solve part1.
part1 :: [String] -> Int
part1 lines' = sum $ map (eval . toExpression . toPostfix . reverse') lines'

-- | Same as above, but for part2.
toPostfix2 :: String -> String
toPostfix2 infix' = toPostfixToken2 infix' [] []

-- | Same as above, but for part2 (implementing the precedence of '+' over '*').
toPostfixToken2 :: String -> String -> String -> String
toPostfixToken2 (' ':tokens) output operators = toPostfixToken2 tokens output operators
toPostfixToken2 ('+':tokens) output ('+':operators) = toPostfixToken2 tokens (output++['+']) ('+':operators)
toPostfixToken2 ('+':tokens) output operators = toPostfixToken2 tokens output ('+':operators)
toPostfixToken2 ('*':tokens) output ('+':operators) = toPostfixToken2 tokens (output++['+']) ('*':operators)
toPostfixToken2 ('*':tokens) output operators = toPostfixToken2 tokens output ('*':operators)
toPostfixToken2 ('(':tokens) output operators = toPostfixToken2 tokens output ('(':operators)
toPostfixToken2 (')':tokens) output operators = toPostfixToken2 tokens output' operators'
  where
    (output', operators') = toPostfixToken' output operators
toPostfixToken2 (token:tokens) output operators = toPostfixToken2 tokens (output++[token]) operators
toPostfixToken2 [] output [] = output
toPostfixToken2 [] output (o:operators) = toPostfixToken2 [] (output++[o]) operators

-- | Solve part2.
part2 :: [String] -> Int
part2 lines' = sum $ map (eval . toExpression . toPostfix2 . reverse') lines'
