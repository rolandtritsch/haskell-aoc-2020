module Day03.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day03

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate $ part1 (input "./input/Day03p1.txt") (3,1)
  printf "Day03: Part1 (%d, %f)\n" result time
