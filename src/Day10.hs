-- |
-- Problem: <https://adventofcode.com/2020/day/10>
--
-- Solution:
--
-- General - Connect jolts/volts.
--
-- Part 1 - The trick here is to read the problem
-- description carefully. You then realize that you
-- can solve the problem by building the diffs between
-- two consecutive numbers and just count the number of
-- 1 and 3 diffs. Multiply the number of 1s and 3s.
--
-- Part 2 - ???
module Day10 where

import Data.List (sort)
import Util (inputRaw)
import Prelude

type Jolt = Int

input :: String -> [Jolt]
input filename = map read $ lines $ inputRaw filename

part1 :: [Jolt] -> Int
part1 jolts = count 1 * count 3
  where
    count n = length $ filter (== n) diffs
    diffs = map (\(a, b) -> b - a) $ zip (init jolts') (tail jolts')
    jolts' = sort $ jolts ++ [0, (maximum jolts) + 3]

part2 :: [Jolt] -> Int
part2 jolts = length jolts
