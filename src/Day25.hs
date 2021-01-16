-- |
-- Problem: <https://adventofcode.com/2020/day/25>
--
-- Solution:
--
-- General - ???
--
-- Part 1 - ???
--
-- Part 2 - ???
module Day25 where

import Util (inputRaw)
import Prelude

type Keys = (Int, Int)

-- | read the input file.
input :: String -> Keys
input filename = (cardPK, doorPK)
  where
    (cardPK : doorPK : _) = map read $ lines $ inputRaw filename

-- | solve part1.
part1 :: Keys -> Int
part1 (cardPK, _doorPK) = cardPK

-- | solve part2.
part2 :: Keys -> Int
part2 (cardPK, _doorPK) = cardPK
