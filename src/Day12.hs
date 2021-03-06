-- |
-- Problem: <https://adventofcode.com/2020/day/12>
--
-- Solution:
--
-- General - Another "execute instructions" problem. If there is a finite
-- set of instructions you can iterate/fold over the instructions to
-- produce the result. If there are jumps and loops and the end of the
-- program is determined by another condition you probably need to go
-- with a recursion.
--
-- Part 1 - This time around we can build a list of instructions. And can
-- just fold the instructions into a/the result. The fold will start with
-- a position of (0, 0) and when it is done we just need to calc the
-- manhatten distance from the final position.
--
-- Part 2 - ???
module Day12 where

import Util (inputRaw)
import Prelude

type Argument = Int

type Position = (Int, Int)

data Direction = North | South | East | West

data Operation = North' | South' | East' | West' | Left' | Right' | Forward'
  deriving (Eq, Show)

data Instruction = Instruction Operation Argument
  deriving (Eq, Show)

makeOp :: Char -> Operation
makeOp 'N' = North'
makeOp 'S' = South'
makeOp 'E' = East'
makeOp 'W' = West'
makeOp 'L' = Left'
makeOp 'R' = Right'
makeOp 'F' = Forward'
makeOp _ = error "Invalid op"

input :: String -> [Instruction]
input filename = map processLine $ lines $ inputRaw filename
  where
    processLine (op : arg) = Instruction (makeOp op) (read arg)
    processLine _ = error "Invalid line"

manhatten :: Position -> Position -> Int
manhatten origin destination = (abs xDiff) + (abs yDiff)
  where
    (xOrigin, yOrigin) = origin
    (xDestination, yDestination) = destination
    xDiff = xOrigin + xDestination
    yDiff = yOrigin + yDestination

execute :: (Position, Direction) -> Instruction -> (Position, Direction)
execute ((x, y), North) (Instruction Forward' offset) = ((x + offset, y), North)
execute ((x, y), South) (Instruction Forward' offset) = ((x - offset, y), South)
execute ((x, y), East) (Instruction Forward' offset) = ((x, y + offset), East)
execute ((x, y), West) (Instruction Forward' offset) = ((x, y - offset), West)
execute ((x, y), direction) (Instruction North' offset) = ((x + offset, y), direction)
execute ((x, y), direction) (Instruction South' offset) = ((x - offset, y), direction)
execute ((x, y), direction) (Instruction East' offset) = ((x, y + offset), direction)
execute ((x, y), direction) (Instruction West' offset) = ((x, y - offset), direction)
execute ((x, y), North) (Instruction Left' 90) = ((x, y), West)
execute ((x, y), North) (Instruction Left' 180) = ((x, y), South)
execute ((x, y), North) (Instruction Left' 270) = ((x, y), East)
execute ((x, y), North) (Instruction Right' 90) = ((x, y), East)
execute ((x, y), North) (Instruction Right' 180) = ((x, y), South)
execute ((x, y), North) (Instruction Right' 270) = ((x, y), West)
execute ((x, y), South) (Instruction Left' 90) = ((x, y), East)
execute ((x, y), South) (Instruction Left' 180) = ((x, y), North)
execute ((x, y), South) (Instruction Left' 270) = ((x, y), West)
execute ((x, y), South) (Instruction Right' 90) = ((x, y), West)
execute ((x, y), South) (Instruction Right' 180) = ((x, y), North)
execute ((x, y), South) (Instruction Right' 270) = ((x, y), East)
execute ((x, y), East) (Instruction Left' 90) = ((x, y), North)
execute ((x, y), East) (Instruction Left' 180) = ((x, y), West)
execute ((x, y), East) (Instruction Left' 270) = ((x, y), South)
execute ((x, y), East) (Instruction Right' 90) = ((x, y), South)
execute ((x, y), East) (Instruction Right' 180) = ((x, y), West)
execute ((x, y), East) (Instruction Right' 270) = ((x, y), North)
execute ((x, y), West) (Instruction Left' 90) = ((x, y), South)
execute ((x, y), West) (Instruction Left' 180) = ((x, y), East)
execute ((x, y), West) (Instruction Left' 270) = ((x, y), North)
execute ((x, y), West) (Instruction Right' 90) = ((x, y), North)
execute ((x, y), West) (Instruction Right' 180) = ((x, y), East)
execute ((x, y), West) (Instruction Right' 270) = ((x, y), South)
execute _ _ = error "Cannot execute"

part1 :: [Instruction] -> Int
part1 instructions = manhatten origin final
  where
    origin = (0, 0)
    (final, _) = foldl execute (origin, East) instructions

part2 :: [Instruction] -> Int
part2 instructions = length instructions
