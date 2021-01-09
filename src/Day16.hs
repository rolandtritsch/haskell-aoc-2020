{-|
Problem: <https://adventofcode.com/2020/day/16>

Solution:

General - Main problem here is that the input file is difficult
to parse. I broke it up into 3 files (-ranges, -your, -nearby)
and parse these files seperately.

Part 1 - After that we just have to filter out the invalid codes
from all codes (given by the nearby tickes.

Part 2 - ???
-}
module Day16 where

import Prelude
import Util (inputRaw)
import Data.List.Split (splitOn)
import Data.List (nub, sort)

type Description = String
data Range = Range Description [Int]

data Notes = Notes {
    ranges :: [Range],
    myTicket :: [Int],
    nearbyTickets :: [[Int]]
}

input :: String -> Notes
input filename = Notes {ranges = ranges', myTicket = myTicket', nearbyTickets = nearbyTickets'} where
    processLine l = map read $ splitOn "," l
    myTicket' = processLine $ head $ lines $ inputRaw (filename ++ "-your")
    nearbyTickets' = map processLine $ lines $ inputRaw (filename ++ "-nearby")
    ranges' = map processRange $ lines $ inputRaw (filename ++ "-ranges") where 
        processRange r = Range desc ([from..to] ++ [from'..to']) where
            desc = (splitOn ":" r) !! 0
            tokens = splitOn " " ((splitOn ":" r) !! 1)
            (from, to) = (read $ (splitOn "-" (tokens !! 1)) !! 0, read $ (splitOn "-" (tokens !! 1)) !! 1)
            (from', to') = (read $ (splitOn "-" (tokens !! 3)) !! 0, read $ (splitOn "-" (tokens !! 3)) !! 1)

part1 :: Notes -> Int
part1 notes = sum invalid where
    valid = nub $ sort $ foldl (++) [] $ map (\(Range _ r) -> r) (ranges notes)
    nearby = sort $ foldl (++) [] (nearbyTickets notes)
    invalid = filter (\e -> notElem e valid) nearby 

part2 :: Notes -> Int
part2 notes = length (nearbyTickets notes)
