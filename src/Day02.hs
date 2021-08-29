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

-- | The policy and the password to check.
data Password
  = Password
      Int
      -- ^ Minimal number of occurences for character.
      Int
      -- ^ Maximum number of occurences for character.
      Char
      -- ^ The character.
      String
      -- ^ The password.
  deriving (Eq, Show)

-- | Read the input file.
input :: String -> [Password]
input filename = map parse $ lines $ inputRaw filename
  where
    parse line = Password min' max' char' password'
      where
        min' = read $ splitOn "-" (words line !! 0) !! 0
        max' = read $ splitOn "-" (words line !! 0) !! 1
        char' = (splitOn ":" (words line !! 1) !! 0) !! 0
        password' = words line !! 2

-- | XOR the expression.
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

-- | Solve part1.
part1 :: [Password] -> Int
part1 passwords = length compliant
  where
    compliant = filter check passwords
    check pw = number >= min' && number <= max'
      where
        (Password min' max' char' password') = pw
        number = length $ filter (== char') password'

-- | Solve part2.
part2 :: [Password] -> Int
part2 passwords = length compliant
  where
    compliant = filter check passwords
    check pw = xor (char' == pos1) (char' == pos2)
      where
        (Password min' max' char' password) = pw
        pos1 = password !! (min' - 1)
        pos2 = password !! (max' - 1)
