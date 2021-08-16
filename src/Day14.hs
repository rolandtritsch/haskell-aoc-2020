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
-- Part 1 - Just run/fold the operations over the program
-- and when you are done, sum up the memory values.
--
-- Part 2 - Initially this sounded/looked easy: Do the same
-- thing again, but with a different implementation for the
-- execution of the operations.
--
-- But then I as not able to find a way to implement part2
-- with a bitwise mask and had to go back to string manipulation
-- to implement the first masking operation (applyMask).
--
-- Not elegant, but it works.

module Day14 where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits ((.&.), (.|.))
import qualified Data.Map as M
import Util (inputRaw)
import Prelude

type Address = Int

type Value = Int

data Operation = Mask Int Int String | Write Int Int
  deriving (Eq, Show)

data Program = Program
  { counter :: Int,
    mask :: (Int, Int),
    memMask :: String,
    memory :: M.Map Address Value
  }

-- http://pleac.sourceforge.net/pleac_haskell/numbers.html#AEN82
bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where
    c2i c = if c == '0' then 0 else 1

sliceTo :: forall a. Int -> Int -> [a] -> [a]
sliceTo from to as = take (to - from + 1) (drop from as)

input :: String -> [Operation]
input filename = map (\l -> processLine (words l)) contents
  where
    contents = lines $ inputRaw filename
    processLine ("mask" : args) = Mask setMask unsetMask mask'
      where
        mask' = args !! 1
        setMask = bin2dec $ map (\c -> if c == 'X' then '0' else c) mask'
        unsetMask = bin2dec $ map (\c -> if c == 'X' then '1' else c) mask'
    processLine tokens = Write address value
      where
        address = read $ sliceTo 4 ((length (tokens !! 0)) - 2) (tokens !! 0)
        value = read (tokens !! 2)

execute :: Program -> Operation -> Program
execute program (Mask setMask unsetMask _) =
  Program
    { counter = counter program + 1,
      mask = (setMask, unsetMask),
      memMask = [],
      memory = memory program
    }
execute program (Write addr value) =
  Program
    { counter = counter program + 1,
      mask = mask program,
      memMask = [],
      memory = M.insert addr maskedValue (memory program)
    }
  where
    (setMask, unsetMask) = mask program
    maskedValue = unsetMask .&. (value .|. setMask)

part1 :: [Operation] -> Int
part1 operations = sum $ M.elems (memory done)
  where
    program = Program {counter = 0, mask = (0,0), memMask = [], memory = M.empty}
    done = foldl execute program operations

dec2bin :: Int -> String
dec2bin n = showIntAtBase 2 intToDigit n ""

prepend :: Char -> Int -> String -> String
prepend c l s
  | l == length s = s
  | otherwise = prepend c l (c : s)

applyMask :: String -> String -> String
applyMask addr' mask' = go addr'' mask''
  where
    l = max (length addr') (length mask')
    addr'' = prepend '0' l addr' 
    mask'' = prepend '0' l mask'
    go (a:as) (m:ms)
      | m == 'X' = 'X' : go as ms
      | m == '1' = '1' : go as ms
      | otherwise = a : go as ms
    go _ _ = []

buildMasks :: String -> [String]
buildMasks [] = [""]
buildMasks (d:ds)
    | d == 'X' = map ('0':) (buildMasks ds) ++ map ('1':) (buildMasks ds)
    | otherwise = map (d:) (buildMasks ds)

execute' :: Program -> Operation -> Program
execute' program (Mask _ _ memMask') =
  Program
    { counter = counter program + 1,
      mask = (0,0),
      memMask = memMask',
      memory = memory program
    }
execute' program (Write addr value) =
  Program
    { counter = counter program + 1,
      mask = (0,0),
      memMask = memMask program,
      memory = updateMemory addr value (memory program) (memMask program)
    }
  where
    updateMemory addr' value' memory' memMask'  = foldl update memory' addresses
      where
        addresses = map bin2dec $ buildMasks $ applyMask (dec2bin addr') memMask'
        update m a = M.insert a value' m  

part2 :: [Operation] -> Int
part2 operations = sum $ M.elems (memory done)
  where
    program = Program {counter = 0, mask = (0,0), memMask = [], memory = M.empty}
    done = foldl execute' program operations
