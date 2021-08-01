module Day10Spec where

import Day10
import Test.Hspec

run :: IO ()
run = hspec $ do
  let jolts = input "./input/Day10p1test.txt"
  let jolts' = input "./input/Day10p1.txt"
  
  describe "input" $ do
    it "input" $ do
      let expected = [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22]
      jolts `shouldBe` expected

  describe "diffs" $ do
    it "testcases" $ do
      let expected = [1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3]
      diffs jolts `shouldBe` expected
              
  describe "part1" $ do
    it "testcases" $ do
      part1 jolts `shouldBe` 35

    it "puzzle" $ do
      part1 jolts' `shouldBe` 2664

  describe "arrangements" $ do
    it "testcases" $ do
      let expected =
            [
              [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 12, 15, 16, 19, 22]
            ]
      arrangements jolts `shouldBe` expected

  describe "valid" $ do
    it "testcases" $ do
      let toBeTested =
            [
              [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 12, 15, 16, 19, 22]
            ]
      all valid toBeTested `shouldBe` True      

  describe "part2" $ do
    it "testcases" $ do
      part2 jolts `shouldBe` 13

    it "puzzle" $ do
      part2 jolts' `shouldBe` 110
