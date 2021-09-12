module Day22Spec where

import Day22
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (Game player1 _) = input "./input/Day22p1test.txt"
      length player1 `shouldBe` 5

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day22p1test.txt") `shouldBe` 306

    it "puzzle" $ do
      part1 (input "./input/Day22p1.txt") `shouldBe` 32489

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day22p1test.txt") `shouldBe` 5

    it "puzzle" $ do
      part2 (input "./input/Day22p1.txt") `shouldBe` 25
