{-|
Problem: <https://adventofcode.com/2020/day/8>

Solution:

General - A(nother) stack machine problem. Execute instructions
until you have reached the end of the program.

Part 1 - The end of the program is reached, when I am hitting
a program counter the second time.

Part 2 - ???
-}
module Day08 where

import Prelude

import Data.List (init, (!!), elem)
import Data.String (lines, split)
import Data.Binary (toStr)

data Code = NOP | ACC | JMP | ERR

data Instruction = Instruction Code Integer

type Program = {
  counter :: Integer,
  register :: Integer,
  instructions :: [Instruction]
}

makeCode :: String -> Code
makeCode "nop" = NOP
makeCode "acc" = ACC
makeCode "jmp" = JMP
makeCode _ = ERR

input :: String -> Program
input filename = {counter = 0, register = 0, instructions = instructions} where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    instructions = map processLine contents where
        processLine l = Instruction code argument where
            tokens = split l " "
            code = makeCode (tokens !! 0)
            argument = read (tokens !! 1)

execute :: Instruction -> Program -> Program
execute (Instruction NOP _) program = {
    counter = program.counter + 1,
    register = program.register,
    instructions = program.instructions
}
execute (Instruction ACC increment) program = {
    counter = program.counter + 1,
    register = program.register + increment,
    instructions = program.instructions
}
execute (Instruction JMP offset) program = {
    counter = program.counter + offset,
    register = program.register,
    instructions = program.instructions
}
execute _ program = {
    counter = 0,
    register = 0,
    instructions = []
}

run :: Program -> [Integer] -> Integer
run program stack
    | elem program.counter stack = program.register
    | otherwise = run next (stack ++ [program.counter]) where
        next = execute (program.instructions !! program.counter) program

part1 :: Program -> Integer
part1 program = run program []

part2 :: Program -> Integer
part2 program = length program.instructions 
