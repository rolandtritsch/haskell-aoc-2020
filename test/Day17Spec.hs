module Day17Spec where

import Day17
import Test.Hspec

import qualified Data.Set as S

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      S.size (input "./input/Day17p1test.txt") `shouldBe` 5

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day17p1test.txt") `shouldBe` 112

    it "puzzle" $ do
      part1 (input "./input/Day17p1.txt") `shouldBe` 333

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day17p1test.txt") `shouldBe` 5

    it "puzzle" $ do
      part2 (input "./input/Day17p1.txt") `shouldBe` 38
