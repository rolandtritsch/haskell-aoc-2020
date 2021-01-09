{-|
Problem: <https://adventofcode.com/2020/day/17>

Solution:

General - There is good news and bad news. The good news is that
the first half of the problem is easy to solve: We just maintain
a set of active coordinates and check/filter in every cycle, if
the coordinates remain active.

The second half is harder to solve/check, because the number of
inactive coordinates is indefinite. We need to find a way to
limit the search space.

One way to do this would/could be to look for the min/max of
x/y/z, add 1 in every direction, create all possible coordinates,
remove the ones that are active and then check the remaining ones.

That can be(come) a big problem fast.

Another way to do it would be to start with the active coordinates
and declare all coordinates around them inactive, remove the active
ones and check the remaining ones.

That sounds more managable. Let's go with that.

Part 1 - Just cycle 6 times.

Part 2 - ???
-}
module Day17 where

import Prelude

import Data.List (nub)
import qualified Data.Set as S
import Util (inputRaw)

type Coordinate = (Int, Int, Int)
type Pocket = S.Set Coordinate

input :: String -> Pocket
input filename = S.delete (-1, -1, -1) $ S.fromList pocket where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    xMax = (length contents) - 1
    yMax = (length (contents !! 0)) - 1
    xLines = zip [0..xMax] contents
    pocket = foldl (++) [] $ map processLines xLines where
        processLines (x, l) = map processCols yCols where
            yCols = zip [0..yMax] l
            processCols (y, '#') = (x, y, 0)
            processCols (y, _) = (-1, -1, -1)

neighborOffsets :: [Coordinate]
neighborOffsets = [(x,y,z) | x <- [-1..1], y <- [-1..1], z <- [-1..1]]

cycle :: Pocket -> Pocket
cycle pocket = S.fromList (nextActive ++ nextActive') where
    active = S.toList pocket
    checkNeighbor (x,y,z) (x',y',z')
        | S.member (x+x', y+y', z+z') pocket = 1
        | otherwise = 0
    count c = sum $ map (\o -> checkNeighbor c o) neighborOffsets
    nextActive = filter remainsActive active where
        -- Note: I need +1 here, because the centre cube itself is active too
        remainsActive c = count' == (2+1) || count' == (3+1) where
            count' = count c
    nextActive' = filter becomesActive inactive where
        becomesActive c = count c == 3
        possibleInactive = nub $ foldl (++) [] $ map (\c -> makeCoordinates c neighborOffsets) active where
            makeCoordinates (x,y,z) offsets = map (\(x',y',z') -> (x+x',y+y',z+z')) offsets
        inactive = filter (\e -> notElem e active) possibleInactive
        
part1 :: Pocket -> Int
part1 pocket = S.size $ go (6-1) $ cycle pocket where
    go 0 p = p
    go n p = go (n-1) $ cycle p

part2 :: Pocket -> Int
part2 pocket = S.size pocket
