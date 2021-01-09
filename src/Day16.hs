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

import Data.List (init, sum, nub, notElem, (!!))
import Data.String (lines, split)
import Data.Binary (toStr)

type Description = String
data Range = Range Description [Integer]

type Notes = {
    ranges :: [Range],
    myTicket :: [Integer],
    nearbyTickets :: [[Integer]]
}

input :: String -> Notes
input filename = {ranges = ranges, myTicket = myTicket, nearbyTickets = nearbyTickets} where
    processLine l = map read $ split l ","
    myTicket = processLine $ head $ init $ lines $ toStr $ unsafePerformIO $ readFile (filename ++ "-your")
    nearbyTickets = map processLine contents where
        contents = init $ lines $ toStr $ unsafePerformIO $ readFile (filename ++ "-nearby")
    ranges = map processRange contents where 
        contents = init $ lines $ toStr $ unsafePerformIO $ readFile (filename ++ "-ranges")
        processRange r = Range desc ([from..to] ++ [from'..to']) where
            desc = (split r ":") !! 0
            tokens = split ((split r ":") !! 1) " "
            (from, to) = (read $ (split (tokens !! 1) "-") !! 0, read $ (split (tokens !! 1) "-") !! 1)
            (from', to') = (read $ (split (tokens !! 3) "-") !! 0, read $ (split (tokens !! 3) "-") !! 1)

part1 :: Notes -> Integer
part1 notes = sum invalid where
    valid = nub $ sort $ foldl (++) [] $ map (\(Range _ r) -> r) notes.ranges
    nearby = sort $ foldl (++) [] notes.nearbyTickets
    invalid = filter (\e -> notElem e valid) nearby 

part2 :: Notes -> Integer
part2 notes = length notes.nearbyTickets 
