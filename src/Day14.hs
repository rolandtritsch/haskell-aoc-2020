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

type Address = Int

type Value = Int

data Operation = Mask Int Int [Int] | Write Int Int
  deriving (Eq, Show)

data Program = Program
  { counter :: Int,
    mask :: (Int, Int),
    memMasks :: [Int],
    memory :: M.Map Address Value
  }

-- http://pleac.sourceforge.net/pleac_haskell/numbers.html#AEN82
bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where
    c2i c = if c == '0' then 0 else 1

sliceTo :: forall a. Int -> Int -> [a] -> [a]
sliceTo from to as = take (to - from + 1) (drop from as)

buildMasks :: String -> [String]
buildMasks (m:ms) = go m ms [] []
  where
    go '0' [] s ss = ss ++ [s ++ "0"]
    go '1' [] s ss = ss ++ [s ++ "1"]
    go 'X' [] s ss = ss ++ [s ++ "0"] ++ [s ++ "1"]
    go '0' (m':ms') s ss = go m' ms' (s ++ "0") ss 
    go '1' (m':ms') s ss = go m' ms' (s ++ "1") ss 
    go 'X' (m':ms') s ss = (go m' ms' (s ++ "0") ss) ++ (go m' ms' (s ++ "1") ss)
    go _ _ _ _ = error "buildMasks - go: Unexpected pattern match"
buildMasks [] = []

input :: String -> [Operation]
input filename = map (\l -> processLine (words l)) contents
  where
    contents = lines $ inputRaw filename
    processLine ("mask" : args) = Mask setMask unsetMask memMasks'
      where
        mask' = args !! 1
        setMask = bin2dec $ map (\c -> if c == 'X' then '0' else c) mask'
        unsetMask = bin2dec $ map (\c -> if c == 'X' then '1' else c) mask'
        memMasks' = map bin2dec $ buildMasks mask'
    processLine tokens = Write address value
      where
        address = read $ sliceTo 4 ((length (tokens !! 0)) - 2) (tokens !! 0)
        value = read (tokens !! 2)

execute :: Program -> Operation -> Program
execute program (Mask setMask unsetMask _) =
  Program
    { counter = counter program + 1,
      mask = (setMask, unsetMask),
      memMasks = [],
      memory = memory program
    }
execute program (Write addr value) =
  Program
    { counter = counter program + 1,
      mask = mask program,
      memMasks = [],
      memory = M.insert addr maskedValue (memory program)
    }
  where
    (setMask, unsetMask) = mask program
    maskedValue = unsetMask .&. (value .|. setMask)

part1 :: [Operation] -> Int
part1 operations = sum $ M.elems (memory done)
  where
    program = Program {counter = 0, mask = (0,0), memMasks = [], memory = M.empty}
    done = foldl execute program operations

-- execute' :: Program -> Operation -> Program
-- execute' program (Mask setMask unsetMask memMasks) =
--   Program
--     { counter = counter program + 1,
--       mask = (setMask, unsetMask),
--       memMasks = memMasks,
--       memory = memory program
--     }
-- execute' program (Write addr value) =
--   Program
--     { counter = counter program + 1,
--       mask = mask program,
--       memMasks = memMasks program,
--       memory = updateMemory (memMasks program) (memory program)
--     }
--   where
--     updateMemory memMasks memory = memory

-- part2 :: [Operation] -> Int
-- part2 operations = sum $ M.elems (memory done)
--   where
--     program = Program {counter = 0, mask = (0,0), memMasks = [], memory = M.empty}
--     done = foldl execute' program operations 
