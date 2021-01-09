module Day15.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day15

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate $ (part2 . input) "./input/Day15p1.txt"
  printf "Day15: Part2 (%d, %f)\n" result time
