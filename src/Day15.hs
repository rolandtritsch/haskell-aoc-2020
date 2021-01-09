{-|
Problem: <https://adventofcode.com/2020/day/15>

Solution:

General - Just keep the last time the number was spoken around and
then look it up when you need to.

Note: For debugging I keep the list of turns a number was spoken around
and I also build/keep the list of ages. Might need to optimize this for
part2 (to reduce the mem/stack size). 

Part 1 - Take 2020 turns and return the last number spoken.

Part 2 - ???
-}
module Day15 where

import Prelude

import Data.List (init)
import Data.String (lines)
import Data.Binary (toStr)
import Data.Map as M

type Spoken = M.Map Integer [Integer]

input :: String -> [Integer]
input filename = map read contents where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename

speak :: Integer -> Integer -> Spoken -> Spoken
speak number turn spoken = M.updateWithInit number (\ts -> [turn] ++ ts) [turn] spoken

whatToSay :: Integer -> Integer -> Spoken -> Integer
whatToSay number turn spoken
    | M.notMember number spoken = 0
    | otherwise = turn - lastTime where
        lastTime = head $ M.get number spoken

nextTurn :: Integer -> Spoken -> Integer -> Integer -> [Integer]
nextTurn number _  _ 0 = [number] 
nextTurn number spoken turn turned = nextTurn nextNumber nextSpoken (turn + 1) (turned - 1) ++ [number] where
    nextNumber = whatToSay number turn spoken
    nextSpoken = speak number turn spoken

part1 :: [Integer] -> Integer
part1 initial = head $ nextTurn initialNumber initialSpoken initialTurn initialTurned  where
    initialSpoken = M.fromList $ zip initial [[t]| t <- [1..(length initial)]]
    initialNumber = whatToSay (last initial) (length initial) initialSpoken
    initialTurn = (length initial) + 1
    initialTurned = 2020 - (length initial) - 1

part2 :: [Integer] -> Integer
part2 initial = length initial
