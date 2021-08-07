module Day11Spec where

import qualified Data.Map as M

import Day11
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (Seats _ _ dimensions) = input "./input/Day11p1test.txt"
      dimensions `shouldBe` (10,10)

  describe "makeNeighbors" $ do
    it "testcases" $ do
      let (Seats _ neighbors _) = input "./input/Day11p1test.txt"
      let expected = [(1,'L'),(2,'L'),(1,'L')]
      neighbors M.! (0,0) `shouldBe` expected
      
      let (Seats _ neighbors' _) = input "./input/Day11p2test.txt"
      let expected' = [(3,'#'),(4,'#'),(1,'#'),(5,'#'),(2,'#'),(4,'#'),(3,'#'),(1,'#')]
      neighbors' M.! (4,3) `shouldBe` expected'

      let (Seats _ neighbors'' _) = input "./input/Day11p2test2.txt"
      let expected'' = [(2,'L')]
      neighbors'' M.! (1,1) `shouldBe` expected''

      let (Seats status neighbors''' _) = input "./input/Day11p2test3.txt"
      let expected''' = []
      status M.! (3,3) `shouldBe` 'L'
      neighbors''' M.! (3,3) `shouldBe` expected'''

  -- describe "part1" $ do
  --   it "testcases" $ do
  --     part1 (input "./input/Day11p1test.txt") `shouldBe` 37

  --   it "puzzle" $ do
  --     part1 (input "./input/Day11p1.txt") `shouldBe` 2289

  -- describe "part2" $ do
  --   it "testcases" $ do
  --     part2 (input "./input/Day11p1test.txt") `shouldBe` 26 

  --   it "puzzle" $ do
  --     part2 (input "./input/Day11p1.txt") `shouldBe` 0
