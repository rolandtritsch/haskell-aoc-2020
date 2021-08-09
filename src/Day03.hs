-- |
-- Problem: <https://adventofcode.com/2020/day/3>
--
-- Solution:
--
-- General - (AFAI can see) Hamler does not support streams/lazy lists (yet).
-- Means for this one I will not use a data structure based approach, but
-- will compute the solution :).
--
-- For that we will read the input and will build and array of lines that
-- have the tree positions on it (let's call it a forrest).
--
-- I can then go into that forrest with my current position (x, y) and can
-- check, if I am hitting a tree. Note: y needs to be modolo the line length.
--
-- Then I am repeating this n times (the number of lines in the input file)
-- advancing m steps (3) to the right until I am done (and count the number
-- of trees I am hitting on the way).
--
-- Part 1 - Doing the above.
--
-- Part 2 - Using part1 to do it for all given moves.

module Day03 where

import Util (inputRaw)
import Prelude

type Coordinates = (Int, Int)

type Trees = [Coordinates]

data Forrest = Forrest Int Int Trees
  deriving (Eq, Show)

input :: String -> Forrest
input filename = Forrest xMax yMax trees
  where
    forrestLines = lines $ inputRaw filename
    xMax = length forrestLines - 1
    yMax = length (head forrestLines) - 1
    trees = [(x, y) | x <- [0 .. xMax], y <- findTrees (forrestLines !! x)]
      where
        findTrees treeLine = map fst $ filter (\(_, t) -> t == '#') treePositions
          where
            treePositions = zip [0 .. yMax] treeLine

part1 :: Forrest -> Coordinates -> Int
part1 forrest move = length collisions
  where
    (Forrest xMax yMax trees) = forrest
    collisions = filter inForest steps
      where
        inForest (x, y) = elem (x, mod y (yMax + 1)) trees
    steps = go move (0, 0)
      where
        go (right, down) (x, y)
          | x >= xMax = []
          | otherwise = [(x + down, y + right)] ++ go (right, down) (x + down, y + right)

part2 :: Forrest -> Int
part2 forrest = product slopes
  where
    moves = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    slopes = map (part1 forrest) moves
