-- |
-- Problem: <https://adventofcode.com/2020/day/11>
--
-- Solution:
--
-- General - Seat System. Main problem to solve is to be able to
-- to check the rules for a given seat. What is the best data
-- structure for that? Options are ...
--
-- * 2-dim array of seat status
-- * map of coordinates to status
-- * list of x,y,status triplets
--
-- Operations we need on this data structure are ...
--
-- * lookup status
-- * change status (not really, because we always build a new map)
--
-- We always need two maps, because we need a/the old (unchanged/
-- unmodified) map to build the new map (because the status changes
-- everywhere at the same time).
--
-- It would be cool, if (while we build the new/next map) we could
-- also determine the neighbor count right away, but ... that is not
-- possible (because the descison if a seat is occupied is made using
-- the old/previous map).
--
-- But ... we can first build the new map and then build a helper map,
-- where every cell has a/the neighbor count in it.
--
-- We can then start to do a recursion until the next map is the same
-- as the previous map.
--
-- Part 1 - ???
--
-- Part 2 - ???
module Day11 where

import qualified Data.Map as M
import Util (inputRaw)
import Prelude

type Location = (Int, Int)

type Dimensions = (Int, Int)

type SeatsStatus = M.Map Location Char

type SeatsNeighborCount = M.Map Location Int

data Seats = Seats SeatsStatus SeatsNeighborCount Dimensions

input :: String -> Seats
input filename = Seats seatsStatus seatsNeighbors (rowCount, colCount)
  where
    contents = lines $ inputRaw filename
    rowCount = length contents
    colCount = length $ head contents
    rows = zip [0 .. (rowCount - 1)] contents
    grid = map (\(row, cols) -> (row, zip [0 .. (colCount - 1)] cols)) rows
    seatsStatus = M.fromList $ foldl (++) [] $ map (\(row, cols) -> map (\(col, status) -> ((row, col), status)) cols) grid
    seatsNeighbors = makeNeighbors seatsStatus (rowCount, colCount)

makeNeighbors :: SeatsStatus -> (Int, Int) -> SeatsNeighborCount
makeNeighbors seatsStatus dimensions = M.fromList neighbors
  where
    (rowCount, colCount) = dimensions
    neighbors = [((row, col), calcCount (row, col)) | row <- [0 .. (rowCount - 1)], col <- [0 .. (colCount -1)]]
      where
        offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
        calcCount (row, col) = sum $ map (\(rowOffset, colOffset) -> lookupCount (row + rowOffset, col + colOffset)) offsets
          where
            lookupCount (row', col')
              | row' == -1 || col' == -1 = 0
              | row' == rowCount || col' == colCount = 0
              | otherwise = if seatsStatus M.! (row', col') == '#' then 1 else 0

nextSeats' :: Seats -> Seats
nextSeats' (Seats status neighbors dimensions) = Seats nextStatus nextNeighbors dimensions
  where
    (rowCount, colCount) = dimensions
    nextStatus = M.fromList $ [((row, col), calcStatus (row, col)) | row <- [0 .. (rowCount - 1)], col <- [0 .. (colCount -1)]]
      where
        calcStatus position
          | status M.! position == 'L' && neighbors M.! position == 0 = '#'
          | status M.! position == '#' && neighbors M.! position >= 4 = 'L'
          | otherwise = status M.! position
    nextNeighbors = makeNeighbors nextStatus dimensions

seatingArea :: (Seats -> Seats) -> Seats -> [Char]
seatingArea nextSeats seats@(Seats status _ _) = M.elems doneStatus
  where
    (Seats doneStatus _ _) = go ns (status == statusNs)
      where
        ns@(Seats statusNs _ _) = nextSeats seats
        go seats' True = seats'
        go seats'@(Seats status' _ _) False = go ns' (status' == statusNs')
          where
            ns'@(Seats statusNs' _ _) = nextSeats seats'

part1 :: Seats -> Int
part1 seats = length $ filter (== '#') $ seatingArea nextSeats' seats 

nextSeats'' :: Seats -> Seats
nextSeats'' (Seats status neighbors dimensions) = Seats nextStatus nextNeighbors dimensions
  where
    (rowCount, colCount) = dimensions
    nextStatus = M.fromList $ [((row, col), calcStatus (row, col)) | row <- [0 .. (rowCount - 1)], col <- [0 .. (colCount -1)]]
      where
        calcStatus position
          | status M.! position == 'L' && neighbors M.! position == 0 = '#'
          | status M.! position == '#' && neighbors M.! position >= 4 = 'L'
          | otherwise = status M.! position
    nextNeighbors = makeNeighbors nextStatus dimensions
    
part2 :: Seats -> Int
part2 seats = length $ filter (== '#') $ seatingArea nextSeats'' seats
