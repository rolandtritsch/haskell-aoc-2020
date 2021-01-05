{-|
Problem: <https://adventofcode.com/2020/day/5>

Solution:

General - Good one. Simple recurrsion problem. Take a range of N and binary
walk/search it (lower, upper) until you find/hit the element.

   N
 /  \
L    U 

A path has the range and the steps in it. A boarding pass has two pathes: A
row path and a column path.

Part 1 - Walk the paths for the boarding pathes and find the max of all seat ids.

Part 2 - Find the one seat that is not on the plane.
-}
module Day05 where

import Prelude

import Data.List (init, maximum, minimum, find, notElem)
import Data.String (lines)
import Data.Binary (toStr)
import Data.Regex as R
import Data.Result (fromOk)
import Data.Maybe (fromJust)

data Step = Lower | Upper
data Range = Range Integer Integer
data Path = Path Range [Step]

type BoardingPass = {
    row :: Path,
    col :: Path
}

input :: String -> [BoardingPass]
input filename = map makeBoardingPass contents where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    makeBoardingPass line = {row = Path (Range 0 127) rowSteps, col = Path (Range 0 7) colSteps} where
        linePattern = fromOk $ R.compile "^((F|B){7})((L|R){3})$" []
        (rowPath, colPath) = case tail $ R.split line linePattern [R.Trim] of
            [rp|[_|[cp|_]]] -> (rp, cp)
            _ -> ("","")
        rowSteps = map makeStep rowPath where
            makeStep 'F' = Lower
            makeStep 'B' = Upper
            makeStep _ = Upper -- Never happening. Protected by regex above.
        colSteps = map makeStep colPath where
            makeStep 'L' = Lower
            makeStep 'R' = Upper
            makeStep _ = Upper -- Never happening. Protected by regex above.

makeSeatId :: BoardingPass -> Integer
makeSeatId boardingPass = rowId * 8 + colId where
    rowId = walkPath boardingPass.row
    colId = walkPath boardingPass.col

walkPath :: Path -> Integer
walkPath (Path (Range from to) [Lower|steps]) = walkPath (Path (Range from ((from+to)/2)) steps)
walkPath (Path (Range from to) [Upper|steps]) = walkPath (Path (Range ((from+to)/2+1) to) steps)
walkPath (Path (Range from _) []) = from

part1 :: [BoardingPass] -> Integer
part1 bs = maximum seatIds where
    seatIds = map makeSeatId bs

part2 :: [BoardingPass] -> Integer
part2 bs = fromJust $ find (\s -> notElem s seatIds) allSeats where
    seatIds = map makeSeatId bs
    allSeats = [(minimum seatIds) .. (maximum seatIds)]
