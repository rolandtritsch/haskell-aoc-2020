{-|
Problem: <https://adventofcode.com/2020/day/0>

Solution:

General - ???

Part 1 - ???

Part 2 - ???
-}
module Day00 where

import Prelude hiding (Word)

import Text.Megaparsec (manyTill, many, eof, optional)
import Text.Megaparsec.Char (newline, alphaNumChar)

import Util (inputRaw, Parser)

type Word = String
type Words = [Word]

-- | read the input file
input :: [String]
input = (lines . inputRaw) "input/Day00p1.txt"

-- | parse the input
parseWords :: Parser Words
parseWords = manyTill (parseWord <* optional newline) eof

parseWord :: Parser Word
parseWord = many alphaNumChar

-- | solve part1.
part1 :: [String] -> Integer
part1 _ = 1

-- | solve the part2.
part2 :: [String] -> Integer
part2 _ = 2

