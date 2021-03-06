module Day14.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day14

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate $ (part1 . input) "./input/Day14p1.txt"
  printf "Day14: Part1 (%d, %f)\n" result time
