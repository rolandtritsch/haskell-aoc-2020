-- |
-- Problem: <https://adventofcode.com/2020/day/15>
--
-- Solution:
--
-- General - Just keep the last time the number was spoken around and
-- then look it up when you need to.
--
-- Note: For debugging I keep the list of turns a number was spoken around
-- and I also build/keep the list of ages. Might need to optimize this for
-- part2 (to reduce the mem/stack size).
--
-- Part 1 - Take 2020 turns and return the last number spoken.
--
-- Part 2 - ???
module Day15 where

import qualified Data.Map as M
import Util (inputRaw)
import Prelude

type Number = Int

type Turn = Int

type Spoken = M.Map Number [Turn]

input :: String -> [Number]
input filename = map read $ lines $ inputRaw filename

speak :: Number -> Turn -> Spoken -> Spoken
speak number turn spoken = M.insertWith (++) number [turn] spoken

whatToSay :: Number -> Turn -> Spoken -> Number
whatToSay number turn spoken
  | M.notMember number spoken = 0
  | otherwise = turn - lastTime
  where
    lastTime = head $ spoken M.! number

nextTurn :: Number -> Spoken -> Turn -> Turn -> [Number]
nextTurn number _ _ 0 = [number]
nextTurn number spoken turn turned = nextTurn nextNumber nextSpoken (turn + 1) (turned - 1) ++ [number]
  where
    nextNumber = whatToSay number turn spoken
    nextSpoken = speak number turn spoken

part1 :: [Number] -> Number
part1 initial = head $ nextTurn initialNumber initialSpoken initialTurn initialTurned
  where
    initialSpoken = M.fromList $ zip initial [[t] | t <- [1 .. (length initial)]]
    initialNumber = whatToSay (last initial) (length initial) initialSpoken
    initialTurn = length initial + 1
    initialTurned = 2020 - length initial - 1

part2 :: [Number] -> Number
part2 initial = length initial
