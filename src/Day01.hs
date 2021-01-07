-- |
-- Problem: <https://adventofcode.com/2020/day/1>
--
-- Solution:
--
-- General - List combination/search problem. Build all combination
-- of list elements. And lock for the ones that sum up to be 2020.
--
-- Part 1 - Do it with pairs.
--
-- Part 2 - Do it with triplets. Note: To make the search space small,
-- we first search the expenses in reverse order and then stop searching,
-- when the first 2 numbers are already bigger than 2020.
module Day01 where

import Util (inputRaw)

type Expense = Int

-- | read the input file.
input :: String -> [Expense]
input = map read . lines . inputRaw

part1 :: [Expense] -> Int
part1 expenses = x * y
  where
    (x, y) = head $ [(x', y') | x' <- expenses, y' <- expenses, x' + y' == 2020]

part2 :: [Expense] -> Int
part2 expenses = x * y * z
  where
    (x, y, z) = head $ [(x', y', z') | x' <- expenses, y' <- expenses, x' + y' <= 2020, z' <- expenses, x' + y' + z' == 2020]
