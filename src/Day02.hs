-- |
-- Problem: <https://adventofcode.com/2020/day/2>
--
-- Solution:
--
-- General - Main problem was parsing the input file :).
--
-- Part 1 - After you have the data, solving the puzzle becomes easy.
--
-- Part 2 - Implemented xor for that.
module Day02 where

import Data.List.Split (splitOn)
import Util (inputRaw)
import Prelude

data Password = Password Int Int Char String
  deriving (Eq, Show)

input :: String -> [Password]
input filename = map parse $ lines $ inputRaw filename
  where
    parse line = Password min' max' char' password'
      where
        min' = read $ splitOn "-" (words line !! 0) !! 0
        max' = read $ splitOn "-" (words line !! 0) !! 1
        char' = (splitOn ":" (words line !! 1) !! 0) !! 0
        password' = words line !! 2

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

part1 :: [Password] -> Int
part1 passwords = length compliant
  where
    compliant = filter check passwords
    check pw = number >= min' && number <= max'
      where
        (Password min' max' char' password') = pw
        number = length $ filter (== char') password'

part2 :: [Password] -> Int
part2 passwords = length compliant
  where
    compliant = filter check passwords
    check pw = xor (char' == pos1) (char' == pos2)
      where
        (Password min' max' char' password) = pw
        pos1 = password !! (min' - 1)
        pos2 = password !! (max' - 1)
