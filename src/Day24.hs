{-# LANGUAGE FlexibleContexts #-}

-- |
-- Problem: <https://adventofcode.com/2020/day/24>
--
-- Solution:
--
-- General - To solve this you have to think about (or google) how to
-- navigate/walk a hex-grid (https://trits.ch/3bJeNeZ).
--
-- To solve this we walk all the steps for any given tile and will end
-- up with a/the list of destination tiles.
--
-- Part 1 - Look for all destination tiles that are there/where flipped
-- an odd number of times.
--
-- Part 2 - ???
module Day24 where

import Data.List (group, sort)
import Data.Text (pack, unpack)
import Text.Regex.Pcre2 (matchAll)
import Util (inputRaw)
import Prelude

type Step = String
type Steps = [Step]
type Tiles = [Steps]

data Position = Position Int Int
  deriving (Eq, Ord, Show)

-- | Read the input file and return the paths to the tiles to turn.
input :: String -> Tiles
input filename = map processTile $ lines $ inputRaw filename
  where
    processTile line = map unpack $ matchAll (pack "(e|se|sw|w|nw|ne)") (pack line)

-- | Walk a step (until there are no more steps) and (at the end)
-- return the final position.
walk :: Steps -> Position -> Position
walk [] position = position
walk ("e" : steps) (Position x y) = walk steps (Position (x + 1) y)
walk ("se" : steps) (Position x y) = walk steps (Position (x + 1) (y -1))
walk ("sw" : steps) (Position x y) = walk steps (Position x (y -1))
walk ("w" : steps) (Position x y) = walk steps (Position (x -1) y)
walk ("nw" : steps) (Position x y) = walk steps (Position (x -1) (y + 1))
walk ("ne" : steps) (Position x y) = walk steps (Position x (y + 1))
walk _ _ = error "Invalid step"

-- | Take a list of tile paths, flip them over and return a/the list of
-- black tiles/positions.
blackTiles :: Tiles -> [Position]
blackTiles tiles = concat $ map snd $ filter (\(n, _) -> odd n) groupedByCount
  where
    destinationTiles = map (\steps -> walk steps (Position 0 0)) tiles
    groupedByCount = map (\positions -> (length positions, positions)) $ group $ sort destinationTiles

-- | Solve part1.
part1 :: Tiles -> Int
part1 tiles = length $ blackTiles tiles 

-- | Solve part2.
part2 :: Tiles -> Int
part2 = length
