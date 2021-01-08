{-|
Problem: <https://adventofcode.com/2020/day/10>

Solution:

General - Connect jolts/volts. The trick here is to read the problem
description carefully. You then realize that you can solve the problem
by building the diffs between 2 consecutive numbers and just count the
number of 1 and 3 diffs.

Part 1 - Multiply the number of ones and threes.

Part 2 - ???
-}
module Day10 where

import Prelude

import Data.List (init, maximum)
import Data.String (lines)
import Data.Binary (toStr)

type Jolt = Integer

input :: String -> [Jolt]
input filename = map read contents where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename

part1 :: [Jolt] -> Integer
part1 jolts = count 1 * count 3 where
    count n = length $ filter (\d -> d == n) diffs where
        jolts' = sort $ jolts ++ [0, (maximum jolts) + 3]
        diffs = map (\(a, b) -> b - a) $ zip (init jolts') (tail jolts')

part2 :: [Jolt] -> Integer
part2 jolts = length jolts
