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

import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (fromJust)
import Data.List (find)
import Util (inputRaw)
import Prelude

data Step = Lower | Upper
data Range = Range Int Int
data Path = Path Range [Step]

data BoardingPass = BoardingPass Path Path

input :: String -> [BoardingPass]
input filename = map makeBoardingPass $ lines $ inputRaw filename where
    makeBoardingPass line = BoardingPass (Path (Range 0 127) rowSteps) (Path (Range 0 7) colSteps) where
        linePattern = mkRegex "^((F|B)*)((L|R)*)$"
        (Just (rowPath:_:colPath:_)) = matchRegex linePattern line
        rowSteps = map makeStep rowPath where
            makeStep 'F' = Lower
            makeStep 'B' = Upper
            makeStep _ = Upper -- Never happening. Protected by regex above.
        colSteps = map makeStep colPath where
            makeStep 'L' = Lower
            makeStep 'R' = Upper
            makeStep _ = Upper -- Never happening. Protected by regex above.

makeSeatId :: BoardingPass -> Int
makeSeatId (BoardingPass row col) = rowId * 8 + colId where
    rowId = walkPath row
    colId = walkPath col

walkPath :: Path -> Int
walkPath (Path (Range from to) (Lower:steps)) = walkPath (Path (Range from (div (from+to) 2)) steps)
walkPath (Path (Range from to) (Upper:steps)) = walkPath (Path (Range (div (from+to) 2+1) to) steps)
walkPath (Path (Range from _) []) = from

part1 :: [BoardingPass] -> Int
part1 bs = maximum seatIds where
    seatIds = map makeSeatId bs

part2 :: [BoardingPass] -> Int
part2 bs = fromJust $ find (\s -> notElem s seatIds) allSeats where
    seatIds = map makeSeatId bs
    allSeats = [(minimum seatIds) .. (maximum seatIds)]
