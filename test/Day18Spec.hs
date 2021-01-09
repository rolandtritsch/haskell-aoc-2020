module Day18Spec where

import Day18
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let expected = Expression Val 0
      head (input "./input/Day18p1test.txt") `shouldBe` expected

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day18p1test.txt") `shouldBe` 26457

    it "puzzle" $ do
      part1 (input "./input/Day18p1.txt") `shouldBe` 209335026987

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day18p1test.txt") `shouldBe` 1

    it "puzzle" $ do
      part2 (input "./input/Day18p1.txt") `shouldBe` 5
