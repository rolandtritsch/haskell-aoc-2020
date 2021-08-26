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
-- Part 2 - ???
module Day16 where

import Data.List (find, findIndex, isInfixOf, nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Util (inputRaw)

import Prelude

type Description = String

type Field = Int

type Ranges = M.Map Description [Field]

data Notes = Notes
  { ranges :: Ranges,
    myTicket :: [Field],
    nearbyTickets :: [[Field]]
  }
  deriving (Eq, Show)

-- | returns the parsed notes
input :: String -> Notes
input filename = Notes {ranges = ranges', myTicket = myTicket', nearbyTickets = nearbyTickets'}
  where
    processLine l = map read $ splitOn "," l
    myTicket' = processLine $ head $ lines $ inputRaw (filename ++ "-your")
    nearbyTickets' = map processLine $ lines $ inputRaw (filename ++ "-nearby")
    ranges' = foldl processRange M.empty $ lines $ inputRaw (filename ++ "-ranges")
      where
        processRange rs r = M.insert desc ([from .. to] ++ [from' .. to']) rs
          where
            desc = (splitOn ":" r) !! 0
            tokens = splitOn " " ((splitOn ":" r) !! 1)
            (from, to) = (read $ (splitOn "-" (tokens !! 1)) !! 0, read $ (splitOn "-" (tokens !! 1)) !! 1)
            (from', to') = (read $ (splitOn "-" (tokens !! 3)) !! 0, read $ (splitOn "-" (tokens !! 3)) !! 1)

-- | returns invalid fields
invalidFields :: Notes -> [Field] 
invalidFields notes = filter (\e -> notElem e valid) nearby
  where
    valid = nub $ concat $ M.elems $ ranges notes
    nearby = concat $ nearbyTickets notes

-- | solving part1
part1 :: Notes -> Int
part1 notes = sum $ invalidFields notes

-- | returns valid tickets
validTickets :: [Field] -> [[Field]] -> [[Field]]
validTickets invalid nearby = filter (not . isInValid) nearby
  where
    isInValid = any $ flip elem invalid

-- | returns index of given range desc
rangeIndex :: [[Field]] -> [Field] -> Int
rangeIndex valid fields = fromJust $ findIndex range (transpose valid)
  where
    range = all (flip elem fields)

-- | returns the range indexes for ranges that have the given word in it
rangeIndexes :: String -> Ranges -> [[Field]] -> [Int]
rangeIndexes word ranges' valid = map (rangeIndex valid) fields
  where
    fields = map ((M.!) ranges') $ filter (isInfixOf word) (M.keys ranges')

-- reduce :: [[Field]] -> Ranges -> ((String, Int), [[Field]])
-- reduce valid ranges = ((desc, col), next)
--   where
--     cols = zip [0..] $ transpose valid

-- | for a given range, check which cols are valid
check :: [Field] -> [(Int, [Field])] -> [(Int, Bool)]
check fields cols = map (\(c, fs) -> (c, all (flip elem fields) fs)) cols

-- | for all ranges, check which cols are valid
check' :: [(Description, [Field])] -> [(Int, [Field])] -> [(Description, [(Int, Bool)])]
check' ranges' cols = map (\(desc, fields) -> (desc, check fields cols)) ranges'

-- | find the (range, col) that is unique
find' :: [(Description, [(Int, Bool)])] -> (Description, Int)
find' checks = (desc, col)
  where
    (desc, cols) = fromJust $ find unique checks
    unique (_, cs) = (length $ filter snd cs) == 1
    (col, _) = fromJust $ find snd cols

-- | (recursively) collect all of the col indexes
collect :: Ranges -> [(Int, [Field])] -> [(Description, Int)] -> [(Description, Int)]
collect ranges' cols' fields'
  | M.size ranges' == 0 = fields'
  | otherwise = collect ranges'' cols'' fields''
  where
    next@(desc, col) = find' $ check' (M.toList ranges') cols'
    ranges'' = M.delete desc ranges'
    cols'' = filter ((/=) col . fst) cols'
    fields'' = next : fields'

-- | returns the fields for the word
solve :: String -> Notes -> [Int]
solve word notes = map ((!!) $ myTicket notes) cidx
   where
     valid = validTickets (invalidFields notes) (nearbyTickets notes)
     cols = zip [0..] $ transpose valid
     cidx = map snd $ filter (isInfixOf word . fst) $ collect (ranges notes) cols []

-- | solves part2
part2 :: Notes -> Int
part2 notes = product $ solve "departure" notes
