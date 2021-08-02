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
-- fits into the device.
--
-- The solution are all paths in the tree.
module Day10 where

import Data.List (sort)
import Util (inputRaw)
import Prelude

type Jolt = Integer

data Node = Node Integer [Node] deriving (Eq, Show)

input :: String -> [Jolt]
input filename = sort $ jolts ++ [0, (maximum jolts) + 3]
  where
    jolts = map read $ lines $ inputRaw filename

diffs :: [Jolt] -> [Integer]
diffs jolts = map (\(a, b) -> b - a) $ zip (init jolts) (tail jolts)

part1 :: [Jolt] -> Integer
part1 jolts = count 1 * count 3
  where
    count n = length $ filter (== n) $ diffs jolts

valid :: [Jolt] -> Bool
valid jolts = all (<= 3) $ diffs jolts

combinations :: Integer -> [Jolt] -> [[Jolt]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations n (j:js) = (map (j:) $ combinations (n-1) js) ++ combinations n js

arrangements :: [Jolt] -> [[Jolt]]
arrangements jolts = filter valid allCombinations
  where
    outlet = head jolts
    device = last jolts
    adapters = tail $ init jolts
    allCombinations = map plugItIn $ init $ combinations (length adapters) adapters
      where
        plugItIn c = [outlet] ++ c ++ [device]

makeTree :: [Jolt] -> Jolt -> Node 
makeTree jolts jolt = Node jolt $ map nextChildren $ nexts jolt 
  where
    nexts c = filter next jolts
      where
        next j = elem j $ map (+c) [1,2,3]
    nextChildren j = makeTree jolts j

allPaths :: [Jolt] -> [[Jolt]] -> Node -> [[Jolt]]
allPaths path paths (Node current []) = [path ++ [current]] ++ paths
allPaths path paths (Node current children) = concatMap next children
  where
    next n = allPaths (path ++ [current]) paths n 

countPaths :: [Jolt] -> Jolt -> Jolt -> Integer -> Integer 
countPaths jolts jolt device count
  | jolt > device = error "Hit circuit breaker (this should never happen)"
  | jolt == device = count + 1
  | otherwise = foldl countNext count $ nexts jolt 
      where
        nexts j = filter next jolts
          where
            next j' = elem j' $ map (+j) [1,2,3]
        countNext a j = countPaths jolts j device a 

part2 :: [Jolt] -> Integer
-- part2 jolts = length $ arrangements jolts
-- part2 jolts = length $ allPaths [] [] $ makeTree jolts 0
part2 jolts = countPaths jolts (head jolts) (last jolts) 0
