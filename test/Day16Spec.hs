module Day16Spec where

import Day16
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let program = input "./input/Day16p1test.txt"
      length (ranges program) `shouldBe` 3
      length (myTicket program) `shouldBe` 3
      length (nearbyTickets program) `shouldBe` 4 

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day16p1test.txt") `shouldBe` 71

    it "puzzle" $ do
      part1 (input "./input/Day16p1.txt") `shouldBe` 21071

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day16p1test.txt") `shouldBe` 4

    it "puzzle" $ do
      part2 (input "./input/Day16p1.txt") `shouldBe` 236
