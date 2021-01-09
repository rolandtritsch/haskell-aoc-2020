{-|
Problem: <https://adventofcode.com/2020/day/18>

Solution:

General - We kinda need to solve two problems here: We need to parse
the expressions and we need to evaluate the expressions.

Looking at the expressions you can see an (binary) abstract syntax tree (AST)
where every node is an expression. The (two) children are either a value
or an expression again.

To build the AST we first convert the expression string from infix notation
to postfix notation (RPN) (with the shunting-yard algorithm).

Building the AST from RPN is trivial (Note: We make this easy by reversing
the RPN (12+ -> +21)).

To evaluate the AST we just have to (recursively) walk the tree.

Part 1 - Evaluate all expressions and sum up the results.

Part 2 - ???
-}
module Day18 where

import Prelude

import Data.List (init, sum)
import Data.String (lines)
import Data.Binary (toStr)
import Data.Char (digitToInt)

data Expression =
    Add Expression Expression |
    Mul Expression Expression |
    Val Integer |
    Nil

pushS :: Char -> String -> String
pushS token stack = [token] ++ stack

popS :: String -> (Char, String)
popS stack = (head stack, tail stack)

pushQ :: Char -> String -> String
pushQ token queue = queue ++ [token]

-- shunting-yard algorithm (toRPN)
toPostfix :: String -> String
toPostfix infix' = go infix' [] [] where
    go [token|tokens] output operators
        | token == ' ' = go tokens output operators
        | token == '+' = go tokens output (pushS token operators)
        | token == '*' = go tokens output (pushS token operators)
        | token == '(' = go tokens output (pushS token operators)
        | token == ')' = go tokens output' operators' where
            (output', operators') = go' output operators where
                go' output'' operators''
                    | head operators'' == '(' = (output'', os) where
                        (_, os) = popS operators''                        
                    | otherwise = go' (pushQ o output'') os where
                        (o, os) = popS operators''
        | otherwise = go tokens (pushQ token output) operators
    go [] output [] = output
    go [] output operators = go [] (pushQ o output) os where
        (o, os) = popS operators

toExpression :: String -> Expression
toExpression postfix = fst $ go (reverse postfix) where
    go [token|tokens]
        | token == '+' = (Add left right, tokens'') where
            (left, tokens') = go tokens
            (right, tokens'') = go tokens'
        | token == '*' = (Mul left right, tokens'') where
            (left, tokens') = go tokens
            (right, tokens'') = go tokens'
        | otherwise = (Val (digitToInt token), tokens)
    go [] = (Nil, [])

-- Note: To make the operators asscociate to the left we just reverse the
-- expression string (and need to flip the parenthesis while doing it).
reverse' :: String -> String
reverse' ['('|tokens] = reverse' tokens ++ [')']
reverse' [')'|tokens] = reverse' tokens ++ ['(']
reverse' [token|tokens] = reverse' tokens ++ [token]
reverse' [] = []

input :: String -> [Expression]
input filename = map processLine contents where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    processLine l = toExpression $ toPostfix $ reverse' l    

eval :: Expression -> Integer
eval (Add e e') = eval e + eval e'
eval (Mul e e') = eval e * eval e'
eval (Val n) = n
eval Nil = 0

size :: Expression -> Integer
size (Add e e') = size e + size e'
size (Mul e e') = size e + size e'
size (Val n) = 1
size Nil = 0

part1 :: [Expression] -> Integer
part1 expressions = sum $ map eval expressions 

part2 :: [Expression] -> Integer
part2 expressions = size $ head expressions
