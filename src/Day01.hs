{- |
Problem: <https://adventofcode.com/2020/day/1>

Solution:

General - List combination/search problem. Build all combination
of list elements. And lock for the ones that sum up to be 2020.

Part 1 - Do it with pairs.

Part 2 - Do it with triplets. Note: To make the search space small,
we first search the expenses in reverse order and then stop searching,
when the first 2 numbers are already bigger than 2020.
-}
module Day01 where

import Text.Megaparsec (manyTill, eof, optional)
import Text.Megaparsec.Char (newline)

import Util (inputRaw, inputParser, Parser, integer)

type Expense = Int

-- | read the input file.
input :: String -> [Expense]
input filename = (map read . lines . inputRaw) filename

-- | the parsed input.
parsedInput :: String -> [Expense]
parsedInput filename = inputParser parseExpenses filename

-- | parse the expenses.
parseExpenses :: Parser [Expense]
parseExpenses = manyTill (parseExpense <* optional newline) eof

parseExpense :: Parser Int
parseExpense = integer

part1 :: [Expense] -> Int
part1 expenses = x * y where
    (x, y) = head $ [(x',y') | x' <- expenses, y' <- expenses, x' + y' == 2020]
    
part2 :: [Expense] -> Int
part2 expenses = x * y * z where
    (x, y, z) = head $ [(x',y',z') | x' <- expenses, y' <- expenses, x' + y' <= 2020, z' <- expenses, x' + y' + z' == 2020]
