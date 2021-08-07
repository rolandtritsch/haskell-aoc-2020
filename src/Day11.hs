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
-- Part 1 - That's what we did for part 1.
--
-- Part 2 - Was much more difficult, because we do not need the adjacent
-- neighbor count, but the count of occupied and empty seats we can see/
-- reach (walking from the current position; over the floor; in all 8
-- directions).

module Day11 where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Util (inputRaw)
import Prelude

type Location = (Int, Int)

type Dimensions = (Int, Int)

type Status = Char

type SeatsStatus = M.Map Location Status

type Distance = (Int, Status) 

type SeatsNeighborDistanceMap = M.Map Location [Distance]

data Seats = Seats SeatsStatus SeatsNeighborDistanceMap Dimensions

input :: String -> Seats
input filename = Seats status neighbors dimensions
  where
    contents = lines $ inputRaw filename
    dimensions@(rowCount, colCount) = (length contents, length $ head contents)
    rows = zip [0 .. (rowCount - 1)] contents
    grid = map (\(row, cols) -> (row, zip [0 .. (colCount - 1)] cols)) rows
    status = M.fromList $ foldl (++) [] $ map (\(row, cols) -> map (\(col, s) -> ((row, col), s)) cols) grid
    neighbors = makeNeighbors status dimensions

makeNeighbors :: SeatsStatus -> Dimensions -> SeatsNeighborDistanceMap
makeNeighbors status dimensions = M.fromList neighbors
  where
    (rowCount, colCount) = dimensions
    upOffsets = [(rowDelta, 0) | rowDelta <- [-1, -2 .. (rowCount - 1)]]
    downOffsets = [(rowDelta, 0) | rowDelta <- [1 .. (rowCount - 1)]]
    leftOffsets = [(0, colDelta) | colDelta <- [-1, -2 .. -(colCount - 1)]]
    rightOffsets = [(0, colDelta) | colDelta <- [0 .. (colCount - 1)]]
    leftUpOffsets = [(rowDelta, colDelta) | rowDelta <- [0, -1 .. -(rowCount - 1)], colDelta <- [0, -1 .. -(colCount - 1)]]
    leftDownOffsets = [(rowDelta, colDelta) | rowDelta <- [0, -1 .. -(rowCount - 1)], colDelta <- [0 .. (colCount - 1)]]
    rightUpOffsets = [(rowDelta, colDelta) | rowDelta <- [0 .. (rowCount - 1)], colDelta <- [0, -1 .. -(colCount - 1)]]
    rightDownOffsets = [(rowDelta, colDelta) | rowDelta <- [0 .. (rowCount - 1)], colDelta <- [0 .. (colCount - 1)]]
    offsets =
      [upOffsets] ++
      [downOffsets] ++
      [leftOffsets] ++
      [rightOffsets] ++
      [leftUpOffsets] ++
      [leftDownOffsets] ++
      [rightUpOffsets] ++
      [rightDownOffsets]
    neighbors = [((row, col), calcDistances (row, col)) | row <- [0 .. (rowCount - 1)], col <- [0 .. (colCount -1)]]
      where
        calcDistances (row, col) = mapMaybe distance offsets
          where
            distance = foldl measure Nothing
              where
                d (rd, cd) = max (abs rd) (abs cd)
                measure Nothing offset@(rowDelta, colDelta)
                  | row + rowDelta >= rowCount || row + rowDelta < 0 = Nothing 
                  | col + colDelta >= colCount || col + colDelta < 0 = Nothing 
                  | status M.! (row + rowDelta, col + colDelta) == '#' = Just(d offset, '#') 
                  | status M.! (row + rowDelta, col + colDelta) == 'L' = Just(d offset, 'L') 
                  | otherwise = Nothing
                measure a _ = a
          
nextSeats' :: Seats -> Seats
nextSeats' (Seats status neighbors dimensions) = Seats nextStatus nextNeighbors dimensions
  where
    (rowCount, colCount) = dimensions
    nextStatus = M.fromList $ [((row, col), calcStatus (row, col)) | row <- [0 .. (rowCount - 1)], col <- [0 .. (colCount -1)]]
      where
        adjacent p = length $ filter (\(d, _) -> d == 1) $ neighbors M.! p
        calcStatus position
          | status M.! position == 'L' && adjacent position == 0 = '#'
          | status M.! position == '#' && adjacent position >= 4 = 'L'
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
nextSeats'' seats = seats
    
part2 :: Seats -> Int
part2 seats = length $ filter (== '#') $ seatingArea nextSeats'' seats
