-- |
-- Problem: <https://adventofcode.com/2020/day/8>
--
-- Solution:
--
-- General - A(nother) stack machine problem. Execute instructions
-- until you have reached the end of the program.
--
-- Part 1 - The end of the program is reached, when I am hitting
-- a program counter the second time. Return the accumulator.
--
-- Part 2 - Need to find a set of instructions that will reach then
-- end of the program (the instruction after the last one). First
-- I am building all possible (better/fixed instruction sets. Then
-- I run them. They will either terminate with LOOP(-detected) or
-- NORMAL (and also return the accumulator). I will then find the
-- first NORMAL termination and return the accumulator of that
-- termination.

module Day08 where

import Data.List (nub, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Util (inputRaw)

data Code = NOP | ACC | JMP
  deriving (Eq, Show)

data Instruction = Instruction Code Int
  deriving (Eq, Show)

data Exit = NORMAL | LOOP
  deriving (Eq, Show)

type Program = (Int, Int)

type Stack = [Int]

makeCode :: String -> Code
makeCode "nop" = NOP
makeCode "acc" = ACC
makeCode "jmp" = JMP
makeCode _ = error "Unknown code"

makeArgument :: String -> Int
makeArgument ('+':arg) = read arg
makeArgument arg = read arg

-- | Returns a/the list of instructions.
input :: String -> [Instruction]
input filename = map makeInstruction $ lines $ inputRaw filename
  where
    makeInstruction l = Instruction (makeCode code) (makeArgument argument)
      where
        (code : argument : _) = splitOn " " l

-- | Execute an instruction and returns the next program (counter/accumulator).
executeInstruction :: Instruction -> Program -> Program
executeInstruction (Instruction NOP _) (counter, accumulator) = (counter + 1, accumulator)
executeInstruction (Instruction ACC increment) (counter, accumulator) = (counter + 1, accumulator + increment)
executeInstruction (Instruction JMP offset) (counter, accumulator) = (counter + offset, accumulator)

-- | Execute all instructions (recursively) until a LOOP is detected
-- or the program terminates normally (hits the instruction after the
-- last one). Also returns the value of the accumulator at the time/point
-- of termination.
runProgram :: [Instruction] -> Program -> Stack -> (Int, Exit)
runProgram instructions program@(counter, accumulator) stack
  | elem counter stack = (accumulator, LOOP)
  | counter == length instructions = (accumulator, NORMAL)
  | otherwise = runProgram instructions next (stack ++ [counter])
  where
    next = executeInstruction (instructions !! counter) program

-- | Flip/swap the first instruction (if it is a NOP/JMP).
fixInstruction :: [Instruction] -> [Instruction]
fixInstruction ((Instruction NOP value):instructions) = Instruction JMP value : instructions
fixInstruction ((Instruction JMP value):instructions) = Instruction NOP value : instructions
fixInstruction instructions = instructions

-- | Build (fixed) instruction lists (from a given instruction list).
buildInstructions :: [Instruction] -> [[Instruction]]
buildInstructions instructions = fixedInstructions where
  fixedInstructions = nub $ map (\(h, t) -> h ++ fixInstruction t) splitInstructions
  splitInstructions = map (\n -> splitAt n instructions) [0..length instructions - 1]

part1 :: [Instruction] -> Int
part1 instructions = accumulator where
  (accumulator, _) = runProgram instructions (0,0) []

part2 :: [Instruction] -> Int
part2 instructions = accumulator where
  (accumulator, _) = fromJust $ find (\(_, exit) -> exit == NORMAL) runs
  runs = map (\is -> runProgram is (0,0) []) (buildInstructions instructions)
