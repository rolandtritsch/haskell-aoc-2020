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
input filename = sort $ jolts ++ [0, (maximum jolts) + 3]
  where
    jolts = map read $ lines $ inputRaw filename

diffs :: [Jolt] -> [Int]
diffs jolts = map (\(a, b) -> b - a) $ zip (init jolts) (tail jolts)

part1 :: [Jolt] -> Int
part1 jolts = count 1 * count 3
  where
    count n = length $ filter (== n) $ diffs jolts

valid :: [Jolt] -> Bool
valid jolts = all (<= 3) $ diffs jolts

combinations :: Int -> [Jolt] -> [[Jolt]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n (j:js) = (map (j:) $ combinations (n-1) js) ++ combinations n js

arrangements :: [Jolt] -> [[Jolt]]
arrangements jolts = filter valid allCombinations
  where
    outlet = head jolts
    device = last jolts
    adapters = tail $ init jolts
    allCombinations = map plugItIn $ init $ combinations (length adapters) adapters
      where
        plugItIn c = [outlet] ++ c ++ [device]

part2 :: [Jolt] -> Int
part2 jolts = length $ arrangements jolts
