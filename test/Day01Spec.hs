module Day01Spec where

import Test.Hspec

import Day01

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      input "./input/Day01p1test.txt" `shouldBe` [1721, 979, 366, 299, 675, 1456] 
      parsedInput "./input/Day01p1test.txt" `shouldBe` [1721, 979, 366, 299, 675, 1456]

  describe "part1" $ do
    it "testcase" $ do
      (part1 . parsedInput) "./input/Day01p1test.txt" `shouldBe` 514579

    it "puzzle" $ do
      (part1 . parsedInput) "./input/Day01p1.txt" `shouldBe` 858496

  describe "part2" $ do
    it "testcase" $ do
      (part2 . parsedInput) "./input/Day01p1test.txt" `shouldBe` 241861950

    it "puzzle" $ do
      (part2 . parsedInput) "./input/Day01p1.txt" `shouldBe` 263819430
