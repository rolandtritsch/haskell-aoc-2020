module Day18.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day18

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate $ (part1 . input) "./input/Day18p1.txt"
  printf "Day18: Part1 (%d, %f)\n" result time
