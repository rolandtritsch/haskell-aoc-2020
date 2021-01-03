module Day00.Part2 where

import System.TimeIt (timeItT)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Day00

-- | main
main :: IO ()
main = do
  (time, result) <- timeItT $ evaluate $ part2 input
  printf "Day00: Part2 (%d, %f)\n" result time
