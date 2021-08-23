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

import Data.List (nub, sort)
import Data.List.Split (splitOn)
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
        processRange a r = M.insert desc ([from .. to] ++ [from' .. to']) a
          where
            desc = (splitOn ":" r) !! 0
            tokens = splitOn " " ((splitOn ":" r) !! 1)
            (from, to) = (read $ (splitOn "-" (tokens !! 1)) !! 0, read $ (splitOn "-" (tokens !! 1)) !! 1)
            (from', to') = (read $ (splitOn "-" (tokens !! 3)) !! 0, read $ (splitOn "-" (tokens !! 3)) !! 1)

-- | returns invalid fields
invalidFields :: Notes -> [Field] 
invalidFields notes = filter (\e -> notElem e valid) nearby
  where
    valid = nub $ sort $ concat $ M.elems (ranges notes)
    nearby = sort $ foldl (++) [] (nearbyTickets notes)

-- | returns valid tickets
validTickets :: [Field] -> [[Field]] -> [[Field]]
validTickets invalid nearby = filter (not . isInValid) nearby
  where
    isInValid = any $ flip elem invalid

part1 :: Notes -> Int
part1 notes = sum $ invalidFields notes

part2 :: Notes -> Int
part2 notes = length (nearbyTickets notes)
