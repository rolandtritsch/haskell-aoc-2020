module Day01.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day01

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate $ (part1 . parsedInput) "./input/Day01p1.txt"
  printf "Day01: Part1 (%d, %f)\n" result time
