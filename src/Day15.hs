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
-- Part 2 - Take 30000000 turns and return the last number spoken.
-- Big surprise (or not): The implementation for part1 is slow, but ...
-- ... not so slow that it is not returning a solution within 2 minutes.
module Day15 where

import qualified Data.Map as M
import Util (inputRaw)
import Prelude

type Number = Int

type Turn = Int

type Spoken = M.Map Number Turn

-- | Read the input file.
input :: String -> [Number]
input filename = map read $ lines $ inputRaw filename

-- | Speak the number (and return the record of spoken numbers).
speak :: Number -> Turn -> Spoken -> Spoken
speak number turn spoken = M.insert number turn spoken

-- | What to say.
whatToSay :: Number -> Turn -> Spoken -> Number
whatToSay number turn spoken
  | M.notMember number spoken = 0
  | otherwise = turn - (spoken M.! number)

-- | What's the next turn?
nextTurn :: Number -> Spoken -> Turn -> Turn -> Number
nextTurn number _ _ 0 = number
nextTurn number spoken turn turned = nextTurn nextNumber nextSpoken (turn + 1) (turned - 1)
  where
    nextNumber = whatToSay number turn spoken
    nextSpoken = speak number turn spoken

-- | Solve the puzzle.
solve :: Turn -> [Number] -> Number
solve turns numbers = nextTurn initialNumber initialSpoken initialTurn initialTurned
  where
    initialSpoken = M.fromList $ zip numbers [1 .. (length numbers)]
    initialNumber = whatToSay (last numbers) (length numbers) initialSpoken
    initialTurn = length numbers + 1
    initialTurned = turns - length numbers - 1

-- | Solve part1.
part1 :: [Number] -> Number
part1 = solve 2020

-- | Solve part2.
part2 :: [Number] -> Number
part2 = solve 30000000
