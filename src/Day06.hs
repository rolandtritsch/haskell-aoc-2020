{-|
Problem: <https://adventofcode.com/2020/day/6>

Solution:

General - Counting problem. 

Part 1 - Counting number of questions per group where anybody (any) said yes.
Sum up the questions.

Part 2 - Counting number of questions per group where everybody (all) said yes.
Sum up the questions.
-}
module Day06 where

import Prelude

import Data.List (sum, nub, group)
import Data.String (split, join, trim)
import Data.Binary (toStr)

type Group = [String]

input :: String -> [Group]
input filename = groups where
    contents = trim $ toStr $ unsafePerformIO $ readFile filename
    groups = map makeGroup $ split contents "\n\n" where
        makeGroup g = split g "\n"
        
part1 :: [Group] -> Integer
part1 gs = sum questions where
    questions = map countYes gs where
        countYes g = length $ nub $ join g ""

part2 :: [Group] -> Integer
part2 gs = sum questions where
    questions = map countYes gs where
        countYes g = length $ filter (\g' -> length g' == length g) $ group $ sort $ join g ""
