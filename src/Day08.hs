-- |
-- Problem: <https://adventofcode.com/2020/day/8>
--
-- Solution:
--
-- General - A(nother) stack machine problem. Execute instructions
-- until you have reached the end of the program.
--
-- Part 1 - The end of the program is reached, when I am hitting
-- a program counter the second time.
--
-- Part 2 - ???
module Day08 where

import Data.List.Split (splitOn)
import Util (inputRaw)
import Prelude

data Code = NOP | ACC | JMP
  deriving (Eq, Show)

data Instruction = Instruction Code Int
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

input :: String -> [Instruction]
input filename = map makeInstruction $ lines $ inputRaw filename
  where
    makeInstruction l = Instruction (makeCode code) (makeArgument argument)
      where
        (code : argument : _) = splitOn " " l

executeInstruction :: Instruction -> Program -> Program
executeInstruction (Instruction NOP _) (counter, register) = (counter + 1, register)
executeInstruction (Instruction ACC increment) (counter, register) = (counter + 1, register + increment)
executeInstruction (Instruction JMP offset) (counter, register) = (counter + offset, register)

runProgram :: [Instruction] -> Program -> Stack -> Int
runProgram instructions program@(counter, register) stack
  | elem counter stack = register
  | otherwise = runProgram instructions next (stack ++ [counter])
  where
    next = executeInstruction (instructions !! counter) program

part1 :: [Instruction] -> Int
part1 instructions = runProgram instructions (0, 0) []

part2 :: [Instruction] -> Int
part2 instructions = length instructions
