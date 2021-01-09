{-# LANGUAGE RankNTypes #-}

-- |
-- Problem: <https://adventofcode.com/2020/day/14>
--
-- Solution:
--
-- General - The crux of this problem is (obviously) to
-- figure out the masking.
--
-- Key is to realize that the given mask is actually two
-- masks: One to set bits and one to unset bits.
--
-- You can then mask the given value with ...
--
-- band setMask (bor value unsetMask)
--
-- Hamler supports bitwise masking on Integers, but
-- (AFAI can see it) has only limited support to work
-- with BitStrings (hence the additional code below
-- to turn a BitString into an Integer).
--
-- Part 1 - Just run/fold the instructions over the program
-- and when you are done, sum up the memory values.
--
-- Part 2 - ???
module Day14 where

import Data.Bits ((.&.), (.|.))
import qualified Data.Map as M
import Util (inputRaw)
import Prelude

type Argument = Int

type Address = Int

type Value = Int

data Operation = Mask | Write
  deriving (Eq, Show)

data Instruction = Instruction Operation Argument Argument
  deriving (Eq, Show)

data Program = Program
  { counter :: Int,
    mask :: (Int, Int),
    memory :: M.Map Address Value
  }

-- http://pleac.sourceforge.net/pleac_haskell/numbers.html#AEN82
bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where
    c2i c = if c == '0' then 0 else 1

sliceTo :: forall a. Int -> Int -> [a] -> [a]
sliceTo from to as = take (to - from + 1) (drop from as)

input :: String -> [Instruction]
input filename = map (\l -> processLine (words l)) contents
  where
    contents = lines $ inputRaw filename
    processLine ("mask" : args) = Instruction Mask setMask unsetMask
      where
        setMask = bin2dec $ map (\c -> if c == 'X' then '0' else c) (args !! 1)
        unsetMask = bin2dec $ map (\c -> if c == 'X' then '1' else c) (args !! 1)
    processLine tokens = Instruction Write address value
      where
        address = read $ sliceTo 4 ((length (tokens !! 0)) - 2) (tokens !! 0)
        value = read (tokens !! 2)

execute :: Program -> Instruction -> Program
execute program (Instruction Mask setMask unsetMask) =
  Program
    { counter = counter program + 1,
      mask = (setMask, unsetMask),
      memory = memory program
    }
execute program (Instruction Write addr value) =
  Program
    { counter = counter program + 1,
      mask = mask program,
      memory = M.insert addr maskedValue (memory program)
    }
  where
    (setMask, unsetMask) = mask program
    maskedValue = unsetMask .&. (value .|. setMask)

part1 :: [Instruction] -> Int
part1 instructions = sum $ M.elems (memory done)
  where
    program = Program {counter = 0, mask = (0, 0), memory = M.empty}
    done = foldl execute program instructions

part2 :: [Instruction] -> Int
part2 instructions = length instructions
