{- |
Problem: <https://adventofcode.com/2020/day/1>

Solution:

General - Simple. Suming up numbers.

Part 1 - Sum up the list of numbers.

Part 2 - Turn the input into in indefinte stream of numbers/frequencies
(using cycle), sum up the sub-lists (with scanl) and (recursively) look
for the first duplicate to show up.
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
