{-|
Problem: <https://adventofcode.com/2020/day/14>

Solution:

General - The crux of this problem is (obviously) to
figure out the masking.

Key is to realize that the given mask is actually two
masks: One to set bits and one to unset bits.

You can then mask the given value with ...

band setMask (bor value unsetMask)

Hamler supports bitwise masking on Integers, but
(AFAI can see it) has only limited support to work
with BitStrings (hence the additional code below
to turn a BitString into an Integer).

Part 1 - Just run/fold the instructions over the program
and when you are done, sum up the memory values.

Part 2 - ???
-}
module Day14 where

import Prelude

import Data.Binary (toStr)
import Data.Float as F
import Data.Int (toFloat)
import Data.List (init, sum, (!!))
import Data.Map as M
import Data.String (lines, words, sliceTo)

type Argument = Integer
type Address = Integer
type Value = Integer
data Operation = Mask | Write
data Instruction = Instruction Operation Argument Argument
type Program = {
    counter :: Integer,
    mask :: (Integer, Integer),
    memory :: M.Map Address Value
}

charToInt0 :: Char -> Integer
charToInt0 '0' = 0
charToInt0 '1' = 1
charToInt0 'X' = 0
charToInt0 _ = -1

charToInt1 :: Char -> Integer
charToInt1 '0' = 0
charToInt1 '1' = 1
charToInt1 'X' = 1
charToInt1 _ = -1

maskStrToBinArray :: (Char -> Integer) -> String -> [Integer]
maskStrToBinArray f m = map f m

binArrayToInteger :: [Integer] -> Integer
binArrayToInteger bin = int where
    aFloat = [toFloat a | a <- reverse bin]
    bFloat = [toFloat b | b <- [0..((length bin) - 1)]] 
    binZip = zip aFloat bFloat
    int = F.trunc $ sum $ map (\(a, b) -> a * F.pow 2.0 b) binZip

input :: String -> [Instruction]
input filename = map (\l -> processLine (words l)) contents where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    processLine ["mask"|args] = Instruction Mask setMask unsetMask where
        setMask = binArrayToInteger $ maskStrToBinArray charToInt0 (args !! 1)
        unsetMask = binArrayToInteger $ maskStrToBinArray charToInt1 (args !! 1)
    processLine tokens = Instruction Write address value where
        address = read $ sliceTo (tokens !! 0) 4 ((length (tokens !! 0)) - 1)
        value = read (tokens !! 2)

execute :: Program -> Instruction -> Program
execute program (Instruction Mask setMask unsetMask) = {
    counter = program.counter + 1,
    mask = (setMask, unsetMask),
    memory = program.memory
}
execute program (Instruction Write addr value) = {
    counter = program.counter + 1,
    mask = program.mask,
    memory = M.put addr maskedValue program.memory
} where
    (setMask, unsetMask) = program.mask
    maskedValue = band unsetMask (bor value setMask)
    
part1 :: [Instruction] -> Integer
part1 instructions = sum $ M.values done.memory where
    program = {counter = 0, mask = (0, 0), memory = M.empty}
    done = foldl execute program instructions

part2 :: [Instruction] -> Integer
part2 instructions = length instructions
