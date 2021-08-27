-- |
-- Problem: <https://adventofcode.com/2020/day/5>
--
-- Solution:
--
-- General - Good one. Simple recursion problem. Take a range of N and binary
-- walk/search it (lower, upper) until you find/hit the element.
--
--    N
--  /  \
-- L    U
--
-- A path has the range and the steps in it. A boarding pass has two pathes: A
-- row path and a column path.
--
-- Right?
--
-- Wrong!
--
-- If you think about it for a sec (or have somebody tell you), you will realize
-- that the boarding pass can be read as a binary number and that the seatId is
-- that binary number.
--
-- Part 1 - Find the max of all seats ids.
--
-- Part 2 - Find the one seat ids that is not on the plane.
module Day05 where

import Data.Char (digitToInt)
import Data.List (find)
import Data.Maybe (fromJust)
import Util (inputRaw)

import Prelude

type SeatId = Int

-- | Read input file.
input :: String -> [SeatId]
input filename = map makeSeatId $ lines $ inputRaw filename
  where
    makeSeatId boardingPass = toInt $ map toBin boardingPass  

-- | Convert characters to binary digits.
toBin :: Char -> Char
toBin 'F' = '0'
toBin 'B' = '1'
toBin 'L' = '0'
toBin 'R' = '1'
toBin _ = error "Unexpected case"

-- | Convert binary string to Int.
toInt :: String -> Int
toInt = foldl (\i c -> i * 2 + digitToInt c) 0

-- | Solve part1.
part1 :: [SeatId] -> Int
part1 seatIds = maximum seatIds

-- | Solve part2.
part2 :: [SeatId] -> Int
part2 seatIds = fromJust $ find (\s -> notElem s seatIds) allSeatIds
  where
    allSeatIds = [(minimum seatIds) .. (maximum seatIds)]
