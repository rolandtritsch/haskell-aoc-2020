{-|
Problem: <https://adventofcode.com/2020/day/11>

Solution:

General - Seat System. Main problem to solve is to be able to
to check the rules for a given seat. What is the best data
structure for that? Options are ...

* 2-dim array of seat status
* map of coordinates to status
* list of x,y,status triplets

Operations we need on this data structure are ...

* lookup status
* change status (not really, because we always build a new map)

We always need two maps, because we need a/the old (unchanged/
unmodified) map to build the new map (because the status changes
everywhere at the same time).

It would be cool, if (while we build the new/next map) we could
also determine the neighbor count right away, but ... that is not
possible (because the descison if a seat is occupied is made using
the old/previous map).

But ... we can first build the new map and then build a helper map,
where every cell has a/the neighbor count in it.

We can then start to do a recursion until the next map is the same
as the previous map.

Part 1 - ???

Part 2 - ???
-}
module Day11 where

import Prelude

import Data.List (init, (!!), sum)
import Data.String (lines)
import Data.Binary (toStr)
import Data.Map as M

type Location = (Integer, Integer)
type SeatsStatus = M.Map Location Char
type SeatsNeighborCount = M.Map Location Integer

type Seats = {
    status :: SeatsStatus,
    neighbors :: SeatsNeighborCount,
    dimensions :: (Integer, Integer)
}

input :: String -> Seats
input filename = {status = seatsStatus, neighbors = seatsNeighbors, dimensions = (rowCount, colCount)} where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    rowCount = length contents
    colCount = length $ contents !! 0
    rows = zip [0..(rowCount - 1)] contents
    grid = map (\(row, cols) -> (row, zip [0..(colCount - 1)] cols)) rows
    seatsStatus = M.fromList $ foldl (++) [] $ map (\(row, cols) -> map (\(col, status) -> ((row, col), status)) cols) grid
    seatsNeighbors = makeNeighbors seatsStatus (rowCount, colCount)

makeNeighbors :: SeatsStatus -> (Integer, Integer) -> SeatsNeighborCount
makeNeighbors seatsStatus dimensions = M.fromList neighbors where
    (rowCount, colCount) = dimensions
    neighbors = [((row, col), calcCount (row, col)) | row <- [0..(rowCount - 1)], col <- [0..(colCount -1)]] where
        offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
        calcCount (row, col) = sum $ map (\(rowOffset, colOffset) -> lookupCount (row + rowOffset, col + colOffset)) offsets where
            lookupCount (row', col')
                | row' == -1 || col' == -1 = 0
                | row' == rowCount || col' == colCount = 0
                | otherwise = if M.get (row', col') seatsStatus == '#' then 1 else 0

nextSeats :: Seats -> Seats
nextSeats seats = {status = nextStatus, neighbors = nextNeighbors, dimensions = seats.dimensions} where
    (rowCount, colCount) = seats.dimensions
    nextStatus = M.fromList $ [((row, col), calcStatus (row, col)) | row <- [0..(rowCount - 1)], col <- [0..(colCount -1)]] where
        calcStatus position
            | M.get position seats.status == 'L' && M.get position seats.neighbors == 0 = '#' 
            | M.get position seats.status == '#' && M.get position seats.neighbors >= 4 = 'L'
            | otherwise = M.get position seats.status
    nextNeighbors = makeNeighbors nextStatus seats.dimensions
    
part1 :: Seats -> Integer
part1 seats = length $ filter (\s -> s == '#') $ M.values done.status where
      done = go ns (seats.status == ns.status) where
          ns = nextSeats seats
          go seats' true = seats'
          go seats' false = go ns'(seats'.status == ns'.status) where
              ns' = nextSeats seats'

part2 :: Seats -> Integer
part2 seats = M.size seats.status
