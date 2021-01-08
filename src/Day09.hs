{-|
Problem: <https://adventofcode.com/2020/day/9>

Solution:

General - Get a/the list of numbers. Set the length of the preamble (P).
Push a window of length P over the list and check the next number (by
calcing the sum of pairs until we find a match).

Part 1 - ???

Part 2 - ???
-}
module Day09 where

import Prelude

import Data.List (init, (!!))
import Data.String (lines)
import Data.Binary (toStr)

type XMAS = {
    preamble :: Integer,
    numbers :: [Integer]
}

input :: String -> XMAS
input filename = {preamble = head contents, numbers = tail contents} where
    contents = map read $ init $ lines $ toStr $ unsafePerformIO $ readFile filename

isNotEmpty :: forall a. [a] -> Bool
isNotEmpty [] = false
isNotEmpty _ = true

check :: [Integer] -> Integer -> Bool
check xmas preamble = isNotEmpty pairs where
    n = xmas !! preamble
    ns = take preamble xmas
    pairs = [(x,y) | x <- ns, y <- ns, x > y, x + y == n] 

part1 :: XMAS -> Integer
part1 xmas = go xmas.numbers xmas.preamble (check xmas.numbers xmas.preamble) where
  go ns p false = ns !! p
  go ns p true = go ns' p (check ns' p) where
      ns' = tail ns

part2 :: XMAS -> Integer
part2 xmas = length xmas.numbers
