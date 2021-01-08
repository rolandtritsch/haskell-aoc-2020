-- |
-- Problem: <https://adventofcode.com/2020/day/13>
--
-- Solution:
--
-- General - This is a reading problem. Reading it fast once,
-- you start to think about lazy list evaluations. Reading it
-- a second time slow(er) you realize that there is an easy
-- way to calculate the solution, because the minutes you
-- need to wait is the remainder of the departure time by
-- the bus id.
--
-- Part 1 - Find the minimum and calculate the solution.
--
-- Part 2 - ???
module Day13 where

import Data.List.Split (splitOn)
import Util (inputRaw)
import Prelude

data Schedule = Schedule
  { departure :: Int,
    busses :: [Int]
  }

input :: String -> Schedule
input filename = Schedule {departure = departure', busses = busses'}
  where
    contents = lines $ inputRaw filename
    departure' = read $ head contents
    busses' = map read $ filter (/= "x") $ splitOn "," (contents !! 1)

part1 :: Schedule -> Int
part1 schedule = minutes * bus
  where
    (minutes, bus) = minimum $ map waitTime (busses schedule)
      where
        waitTime b = ((negate $ mod (departure schedule) b) + b, b)

part2 :: Schedule -> Int
part2 schedule = length (busses schedule)
