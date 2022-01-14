module Day24Spec where

import Day24
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day24p1test.txt") `shouldBe` 20

  describe "walk" $ do
    it "testcases" $ do
      let steps = ["se","se","nw","ne","ne","ne","w","se","e","sw","w","sw","sw","w","ne","ne","w","se","w","sw"]
      walk steps (Position 0 0) `shouldBe` Position (-1) (-2)
                 
  describe "blackTiles" $ do
    it "testcases" $ do
      let expected = [Position (-2) 0,Position (-2) 1,Position (-1) (-2),Position (-1) (-1),Position (-1) 1,Position 0 (-3),Position 0 0,Position 0 3,Position 2 (-2),Position 2 0]
      blackTiles (input "./input/Day24p1test.txt") `shouldBe` expected
                 
  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day24p1test.txt") `shouldBe` 10

    it "puzzle" $ do
      part1 (input "./input/Day24p1.txt") `shouldBe` 269

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day24p1test.txt") `shouldBe` 20

    it "puzzle" $ do
      part2 (input "./input/Day24p1.txt") `shouldBe` 315
