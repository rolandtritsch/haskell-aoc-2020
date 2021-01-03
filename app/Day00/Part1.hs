module Day00.Part1 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day00

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate $ part1 input
  printf "Day00: Part1 (%d, %f)\n" result time
