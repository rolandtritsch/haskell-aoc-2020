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
-- Part 2 - Take the result from part1 and start applying the rules
-- day after day. To implement the rules we need to understand how many
-- black tiles are adjacent to a given (black or white) tile (e.g. (count,
-- position)). We can then apply the rules and flip the tiles accordingly.
--
-- Note: The grid has no boundaries. Means the number of white tiles is
-- indefinte. But the number/list of black tiles is not. Means we will
-- model the grid by means of a list of black tiles (all other tiles
-- are (by definition) white).
module Day24 where

import Data.List (group, sort, nub)
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
walk ("se" : steps) (Position x y) = walk steps (Position (x + 1) (y - 1))
walk ("sw" : steps) (Position x y) = walk steps (Position x (y - 1))
walk ("w" : steps) (Position x y) = walk steps (Position (x - 1) y)
walk ("nw" : steps) (Position x y) = walk steps (Position (x - 1) (y + 1))
walk ("ne" : steps) (Position x y) = walk steps (Position x (y + 1))
walk _ _ = error "Invalid step"

-- | Take a list of tile paths, flip them over and return a/the list of
-- black tiles/positions.
initialBlackTiles :: Tiles -> [Position]
initialBlackTiles tiles = concat $ map snd $ filter (\(n, _) -> odd n) groupedByCount
  where
    destinationTiles = map (\steps -> walk steps (Position 0 0)) tiles
    groupedByCount = map (\positions -> (length positions, positions)) $ group $ sort destinationTiles

-- | Solve part1.
part1 :: Tiles -> Int
part1 tiles = length $ initialBlackTiles tiles

-- | For a given tile, return the number of adjacent black tiles
numberOfAdjacentBlackTiles :: Position -> [Position] -> Int
numberOfAdjacentBlackTiles tile blackTiles = (length . filter (isAdjacent tile)) blackTiles

-- | For a list of tiles, return the number of adjacent black tiles
numberOfAdjacentBlackTiles' :: [Position] -> [Position] -> [(Int, Position)]
numberOfAdjacentBlackTiles' tiles blackTiles = map process tiles
  where
    process tile = (numberOfAdjacentBlackTiles tile blackTiles, tile)

-- | Check the rule and return the qualified tiles.
checkRule :: ((Int, Position) -> Bool) -> [(Int, Position)] -> [Position]
checkRule rule tiles = map snd $ filter rule tiles

-- | Any black tile with zero or more than 2 black tiles immediately
-- adjacent to it needs to be flipped (to white).
blackRule :: (Int, Position) -> Bool
blackRule (n, _)
  | n == 0 || n > 2 = True
  | otherwise = False

-- | Any white tile with exactly 2 black tiles immediately
-- adjacent to it needs to be flipped (to black).
whiteRule :: (Int, Position) -> Bool
whiteRule (n, _)
  | n == 2 = True
  | otherwise = False

-- | Return all adjacent tiles for the given tile.
allAdjacent :: Position -> [Position]
allAdjacent (Position x y) =
  [ Position (x + 1) y,
    Position (x + 1) (y - 1),
    Position x (y - 1),
    Position (x - 1) y,
    Position (x - 1) (y + 1),
    Position x (y + 1)
  ]

-- | Tests, if a two tiles are adjacent.
isAdjacent :: Position -> Position -> Bool
isAdjacent thisTile thatTile = elem thisTile (allAdjacent thatTile)

-- | For the given list of black tiles, return a/the list of white
-- that need to be checked/considered.
allWhiteTiles :: [Position] -> [Position]
allWhiteTiles tiles = nub $ sort $ filter (not . flip elem tiles) $ foldl process [] tiles
  where
    process whites tile = whites ++ allAdjacent tile

-- | Do a day of work.
doDay :: [Position] -> [Position]
doDay tiles = whiteTilesToBeFlippedToBlack ++ filter (not . flip elem blackTilesToBeFlippedToWhite) tiles
  where
    blackTilesToBeFlippedToWhite = checkRule blackRule (numberOfAdjacentBlackTiles' tiles tiles)
    whiteTilesToBeFlippedToBlack = checkRule whiteRule (numberOfAdjacentBlackTiles' (allWhiteTiles tiles) tiles)

-- | Solve part2.
part2 :: Tiles -> Int
part2 tiles = length $ go (100 :: Int) (initialBlackTiles tiles)
 where
   go 0 tiles' = tiles'
   go n tiles' = go (n - 1) (doDay tiles')
