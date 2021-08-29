-- |
-- Problem: <https://adventofcode.com/2020/day/6>
--
-- Solution:
--
-- General - Counting problem.
--
-- Part 1 - Counting number of questions per group where anybody (any) said yes.
-- Sum up the questions.
--
-- Part 2 - Counting number of questions per group where everybody (all) said yes.
-- Sum up the questions.
module Day06 where

import Data.List (group, intercalate, nub, sort)
import Data.List.Split (splitOn)
import Util (inputRaw)
import Prelude

type Group = [String]

-- | Read the input file.
input :: String -> [Group]
input filename = map (splitOn "\n") $ splitOn "\n\n" $ inputRaw filename

-- | Solve part1.
part1 :: [Group] -> Int
part1 gs = sum questions
  where
    questions = map countYes gs
    countYes g = length $ nub $ intercalate "" g

-- | Solve part2.
part2 :: [Group] -> Int
part2 gs = sum questions + 1
  where
    questions = map countYes gs
    countYes g = length $ filter (\g' -> length g' == length g) $ group $ sort $ intercalate "" g
