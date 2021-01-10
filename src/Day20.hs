-- |
-- Problem: <https://adventofcode.com/2020/day/20>
--
-- Solution:
--
-- General - ???
--
-- Part 1 - ???
--
-- Part 2 - ???
module Day20 where

import Util (inputRaw)
import Prelude

input :: String -> [String]
input filename = contents
  where
    contents = lines $ inputRaw filename

part1 :: [String] -> Int
part1 is = length is

part2 :: [String] -> Int
part2 is = length is
