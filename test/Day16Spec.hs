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

  describe "invalidFields" $ do
    it "testcases" $ do
      invalidFields (input "./input/Day16p1test.txt") `shouldBe` [4,12,55]

  describe "validTickets" $ do
    it "testcases" $ do
      let notes = input "./input/Day16p1test.txt"
      validTickets (invalidFields notes) (nearbyTickets notes) `shouldBe` [[7,3,47]]
      
      let notes' = input "./input/Day16p2test.txt"
      validTickets (invalidFields notes') (nearbyTickets notes') `shouldBe` [[3,9,18],[15,1,5],[5,14,9]]

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day16p1test.txt") `shouldBe` 4

    it "puzzle" $ do
      part2 (input "./input/Day16p1.txt") `shouldBe` 236
