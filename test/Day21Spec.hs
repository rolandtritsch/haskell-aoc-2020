module Day21Spec where

import Day21
import Test.Hspec

import qualified Data.Map as M

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      M.size (input "./input/Day21p1test.txt") `shouldBe` 1

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day21p1test.txt") `shouldBe` 5 

    it "puzzle" $ do
      part1 (input "./input/Day21p1.txt") `shouldBe` 2584

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day21p1test.txt") `shouldBe` 1

    it "puzzle" $ do
      part2 (input "./input/Day21p1.txt") `shouldBe` 8
