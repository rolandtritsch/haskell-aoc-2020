-- |
-- Problem: <https://adventofcode.com/2020/day/16>
--
-- Solution:
--
-- General - Main problem here is that the input file is difficult
-- to parse. I broke it up into 3 files (-ranges, -your, -nearby)
-- and parse these files seperately.
--
-- Part 1 - After that we just have to filter out the invalid codes
-- from all codes (given by the nearby tickes).
--
-- Part 2 - This was ... difficult. No big algorithms involved. Just
-- a lot of careful reading and the realisation that finding the colums
-- is recursive (not unique, when you do it in one go). This was also
-- difficult because you where able to make the testcases pass with a
-- much simpler implementation and then it was hard to understand/see
-- why it is not working for the puzzle input.
--
-- But first things first: Let's define data-structures/terminology ...
--
-- * The input has three parts: the ranges of numbers that describe
--   a valid field (or column; see below)
-- * The/My ticket. Every number is a field. We need to find out how
--   to associate these numbers with the ranges above
-- * Nearby tickets to intersect with ranges to find out which field/column
--   belongs to which range
-- * You want to end up with a mapping of range-name to column-number
--
-- Now let's talk about the algorithm ...
--
-- * This problem can be solved by building intersections of number
--   sets (we only need any (not empty intersection) and all (full/complete
--   intersection))
-- * First we need to find all valid tickets. That's kinda easy. We just
--   take the invalid fields from part1 and remove the tickets that have
--   an invalid field in it
-- * Now the real work starts
-- * Next we need to realize that we need to look at the valid (nearby)
--   tickets column by column (and need to find out which column fits/matches
--   which range)
-- * This should be easy, right? We just intersect the column with the ranges
--   and if all the numbers in the column are in the range we have a match
-- * This is when things get tricky. Because this is not the case. The match
--   is not unique. Means a column will match with multiple ranges
-- * BUT ... one of the columns will match with exactly one range. And that's
--   the one you are looking for. You then have your first mapping of a
--   range-name to a column-number
-- * You then need to remove the range from the ranges and the column from
--   the columns and do it again (recursively, until no more ranges/columns
--   need to be mapped
-- * When you are done with the mapping you can just lookup/select the
--   6 column-numbers, lookup the 6 numbers in your ticket and multiply
--   them
-- * Done 
module Day16 where

import Data.List (find, isInfixOf, nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Util (inputRaw)

import Prelude

type Description = String
type Field = Int
type Ranges = M.Map Description [Field]
type Ticket = [Field]
type Column = Int
type Columns = M.Map Column [Field]
type Mappings = M.Map Description Column

-- | The note that describes the ticket. 
data Notes = Notes Ranges Ticket [Ticket]
  deriving (Eq, Show)

-- | Read the input file.
input :: String -> Notes
input filename = Notes ranges myTicket nearbyTickets
  where
    processLine l = map read $ splitOn "," l
    myTicket = processLine $ head $ lines $ inputRaw (filename ++ "-your")
    nearbyTickets = map processLine $ lines $ inputRaw (filename ++ "-nearby")
    ranges = foldl processRange M.empty $ lines $ inputRaw (filename ++ "-ranges")
      where
        processRange rs r = M.insert desc ([from .. to] ++ [from' .. to']) rs
          where
            desc = (splitOn ":" r) !! 0
            tokens = splitOn " " ((splitOn ":" r) !! 1)
            (from, to) = (read $ (splitOn "-" (tokens !! 1)) !! 0, read $ (splitOn "-" (tokens !! 1)) !! 1)
            (from', to') = (read $ (splitOn "-" (tokens !! 3)) !! 0, read $ (splitOn "-" (tokens !! 3)) !! 1)

-- | Returns the invalid fields.
invalidFields :: Notes -> [Field] 
invalidFields (Notes ranges _ nearbyTickets) = invalid
  where
    valid = nub $ concat $ M.elems ranges
    invalid = filter (flip notElem valid) $ concat nearbyTickets 

-- | Solve part1.
part1 :: Notes -> Int
part1 notes = sum $ invalidFields notes

-- | Returns the valid tickets.
validTickets :: [Field] -> [Ticket] -> [Ticket]
validTickets invalid nearbyTickets = filter (not . isInValid) nearbyTickets
  where
    isInValid = any $ flip elem invalid

-- | Check for a given range, which cols are valid.
check :: [Field] -> Columns -> [(Int, Bool)]
check range cols = map (\(c, fs) -> (c, all (flip elem range) fs)) $ M.toList cols

-- | Checks for all ranges, which cols are valid.
checks :: Ranges -> Columns -> [(Description, [(Column, Bool)])]
checks ranges cols = map (\(desc, fields) -> (desc, check fields cols)) $ M.toList ranges

-- | Find the one col, that can be uniquely mapped to a range. 
unique :: [(Description, [(Column, Bool)])] -> (Description, Column)
unique checks' = (desc, col)
  where
    (desc, cols) = fromJust $ find unique' checks'
    unique' (_, cs) = (length $ filter snd cs) == 1
    (col, _) = fromJust $ find snd cols

-- | (Recursively) Collect all of the col indexes.
collect :: Ranges -> Columns -> Mappings -> Mappings
collect ranges cols mappings
  | M.size ranges == 0 = mappings
  | otherwise = collect ranges' cols' mappings'
  where
    (desc, col) = unique $ checks ranges cols
    ranges' = M.delete desc ranges
    cols' = M.delete col cols
    mappings' = M.insert desc col mappings

-- | Returns the fields for the word.
solve :: String -> Notes -> [Int]
solve word notes@(Notes ranges myTicket nearbyTickets) = map ((!!) myTicket) colIndexes
   where
     valid = validTickets (invalidFields notes) nearbyTickets
     cols = M.fromList $ zip [0..] $ transpose valid
     colIndexes = map snd $ filter (isInfixOf word . fst) $ M.toList $ collect ranges cols M.empty

-- | Solve part2.
part2 :: Notes -> Int
part2 notes = product $ solve "departure" notes
