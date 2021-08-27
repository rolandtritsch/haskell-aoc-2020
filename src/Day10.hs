-- |
-- Problem: <https://adventofcode.com/2020/day/10>
--
-- Solution:
--
-- General - Connect jolts/volts.
--
-- Part 1 - The trick here is to read the problem
-- description carefully. You then realize that you
-- can solve the problem by building the diffs between
-- two consecutive numbers and just count the number of
-- 1 and 3 diffs. Multiply the number of 1s and 3s.
--
-- Part 2 - My first attempt ... failed. The idea was
-- to build all possible combinations/arrangements and
-- then just filter for the valid ones. That worked
-- for the testcases, but failed (miserably) for the
-- puzzle input (had to kill it after running it for
-- an hour).
--
-- My second attempt goes about it differently. It builds
-- a tree. It starts with the outlet (0) and then tries
-- to find all adapters that are [1,2,3] jolts away from
-- the current adapter until it finds the adapter that
-- fits into the device. The solution are all paths in
-- the tree, but (again) ... this takes too long.
--
-- My third attempt just counts the paths that I can build,
-- but (you guessed it) ...
--
-- My last attempt was/is based on this thread - https://tedn.ly/s5w.
module Day10 where

import Data.List (genericLength, sort)
import Util (inputRaw)
import Prelude

type Jolt = Int

-- | The tree of Jolts
data Node = Node Jolt [Node]
  deriving (Eq, Show)

-- | Read the input file.
input :: String -> [Jolt]
input filename = sort $ jolts ++ [0, (maximum jolts) + 3]
  where
    jolts = map read $ lines $ inputRaw filename

-- | Returns the diffs between two consecutive jolts.
diffs :: [Jolt] -> [Int]
diffs jolts = map (\(a, b) -> b - a) $ zip (init jolts) (tail jolts)

-- | Solve part1.
part1 :: [Jolt] -> Int
part1 jolts = count 1 * count 3
  where
    count n = length $ filter (== n) $ diffs jolts

-- | A list of jolts is valid, if they are never more that 3 appart.
valid :: [Jolt] -> Bool
valid jolts = all (<= 3) $ diffs jolts

-- | Build all combinations of jolts.
combinations :: Int -> [Jolt] -> [[Jolt]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n (j : js) = (map (j :) $ combinations (n -1) js) ++ combinations n js

-- | Build all possible arrangements.
arrangements :: [Jolt] -> [[Jolt]]
arrangements jolts = filter valid allCombinations
  where
    outlet = head jolts
    device = last jolts
    adapters = tail $ init jolts
    allCombinations = map plugItIn $ init $ combinations (length adapters) adapters
      where
        plugItIn c = [outlet] ++ c ++ [device]

-- | Build a tree (from the given list of jolts) and return the root of the tree.
makeTree :: [Jolt] -> Jolt -> Node
makeTree jolts jolt = Node jolt $ map nextChildren $ nexts jolt
  where
    nexts c = filter next jolts
      where
        next j = elem j $ map (+ c) [1, 2, 3]
    nextChildren j = makeTree jolts j

-- | Build all possible paths (from the given node).
allPaths :: [Jolt] -> [[Jolt]] -> Node -> [[Jolt]]
allPaths path paths (Node current []) = (path ++ [current]) : paths
allPaths path paths (Node current children) = concatMap next children
  where
    next n = allPaths (path ++ [current]) paths n

-- | Count the paths that fit into the device.
countPaths :: [Jolt] -> Jolt -> Jolt -> Integer -> Integer
countPaths jolts jolt device count
  | jolt > device = error "Hit circuit breaker (this should never happen)"
  | jolt == device = count + 1
  | otherwise = foldl countNext count $ nexts jolt
  where
    nexts j = filter next ns
      where
        ns = map (+ j) [1, 2, 3]
        next j' = elem j' jolts
    countNext a j = countPaths jolts j device a

-- | Built combination of paths based on adjacent jolts being equal.
adjacent :: Jolt -> Jolt -> [Int] -> [[Int]] -> [[Int]]
adjacent prev current (r : rs) acc@(a : as)
  | prev == current = adjacent current r rs ((current : a) : as)
  | otherwise = adjacent current r rs ([current] : acc)
adjacent prev current [] acc@(a : as)
  | prev == current = (current : a) : as
  | otherwise = [current] : acc
adjacent _ _ _ _ = error "adjacent: Unexpected pattern match"

-- | Solve part2 (based on counting the arrangements).
part2' :: [Jolt] -> Int
part2' jolts = length $ arrangements jolts

-- | Solve part2 (based on counting all paths in the tree).
part2'' :: [Jolt] -> Int
part2'' jolts = length $ allPaths [] [] $ makeTree jolts 0

-- | Solve part2 (with an optimized version of part2'')
part2''' :: [Jolt] -> Integer
part2''' jolts = countPaths jolts (head jolts) (last jolts) 0

-- | Solve part2.
part2 :: [Jolt] -> Integer
part2 jolts = product possiblePaths
  where
    (p : c : rs) = diffs jolts
    as = adjacent p c rs [[p]]
    ones = map genericLength $ filter (\(i : _) -> i == 1) as
    possiblePaths = map f ones
      where
        f n = 1 + div (n * (n -1)) 2
