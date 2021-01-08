{-|
Problem: <https://adventofcode.com/2020/day/13>

Solution:

General - This is a reading problem. Reading it fast once,
you start to think about lazy list evaluations. Reading it
a second time slow(er) you realize that there is an easy
way to calculate the solution, because the minutes you
need to wait is the remainder of the departure time by
the bus id.

Part 1 - Find the minimum and calculate the solution.

Part 2 - ???
-}
module Day13 where

import Prelude

import Data.List (init, (!!), minimum)
import Data.String (lines, split)
import Data.Binary (toStr)

type Schedule = {
    departure :: Integer,
    busses :: [Integer]
}

input :: String -> Schedule
input filename =  {departure = departure, busses = busses} where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    departure = read $ contents !! 0
    busses = map read $ filter (\b -> b /= "x") $ split (contents !! 1) ","

part1 :: Schedule -> Integer
part1 schedule = minutes * bus where
    (minutes, bus) = minimum $ map waitTime schedule.busses where
        waitTime b = ((negate $ schedule.departure % b) + b, b)  

part2 :: Schedule -> Integer
part2 schedule = length schedule.busses
