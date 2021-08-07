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
      let (Seats status _ dimensions) = input "./input/Day11p2test.txt"
      let expected = [(3,'#'),(4,'#'),(1,'#'),(5,'#'),(2,'#'),(4,'#'),(3,'#'),(1,'#')]
      let neighbors = makeNeighbors status dimensions
      neighbors M.! (4,3) `shouldBe` expected

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day11p1test.txt") `shouldBe` 37

    it "puzzle" $ do
      part1 (input "./input/Day11p1.txt") `shouldBe` 2289

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day11p1test.txt") `shouldBe` 26 

    it "puzzle" $ do
      part2 (input "./input/Day11p1.txt") `shouldBe` 0
