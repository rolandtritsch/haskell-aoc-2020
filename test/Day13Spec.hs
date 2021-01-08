module Day13Spec where

import Day13
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let schedule = input "./input/Day13p1test.txt"
      departure schedule `shouldBe` 0
      length (busses schedule) `shouldBe` 0

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day13p1test.txt") `shouldBe` 295

    it "puzzle" $ do
      part1 (input "./input/Day13p1.txt") `shouldBe` 2305

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day13p1test.txt") `shouldBe` 1

    it "puzzle" $ do
      part2 (input "./input/Day13p1.txt") `shouldBe` 9
