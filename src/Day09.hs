-- |
-- Problem: <https://adventofcode.com/2020/day/9>
--
-- Solution:
--
-- General - Get a/the list of numbers. Set the length of the preamble (P).
-- Push a window of length P over the list and check the next number (by
-- calcing the sum of pairs until we find a match).
--
-- Part 1 - Calc the pairs recursively.
--
-- Part 2 - Use part1 to find the invalidNumber and then (recursively)
-- push a window over the input to find the encryption weakness.

module Day09 where

import Util (inputRaw)
import Prelude

-- | The length of the preamble and the numbers.
data XMAS = XMAS Int [Int]

-- | Read the input file.
input :: String -> XMAS
input filename = XMAS preamble numbers
  where
    (preamble : numbers) = map read $ lines $ inputRaw filename

-- | Return true, if there is at least one valid pair.
check :: [Int] -> Int -> Bool
check xmas preamble = not (null pairs)
  where
    n = xmas !! preamble
    ns = take preamble xmas
    pairs = [(x, y) | x <- ns, y <- ns, x > y, x + y == n]

-- | Solve part1.
part1 :: XMAS -> Int
part1 (XMAS preamble numbers) = go numbers preamble (check numbers preamble)
  where
    go ns p False = ns !! p
    go ns p True = go ns' p (check ns' p)
      where
        ns' = tail ns

-- | Return the encryption weakness.
findEncryptionWeakness :: Int -> [Int] -> (Bool, [Int])
findEncryptionWeakness invalidNumber (n:numbers) = go (n, [n]) numbers
  where
    go (n', ns') (n'':ns'')
      | n' == invalidNumber = (True, ns')
      | n' > invalidNumber = (False, numbers)
      | otherwise = go (n' + n'', ns' ++ [n'']) ns''
    go (n', ns') []
      | n' == invalidNumber = (True, ns')
      | otherwise = (False, numbers)
findEncryptionWeakness _ [] = (False, [])

-- | Solve part2.
part2 :: XMAS -> Int
part2 xmas@(XMAS _ numbers) = minimum encryptionWeakness + maximum encryptionWeakness
  where
    invalidNumber = part1 xmas
    encryptionWeakness = go $ findEncryptionWeakness invalidNumber numbers
      where
        go (False, []) = []
        go (False, rest) = go $ findEncryptionWeakness invalidNumber rest
        go (True, weakness) = weakness
