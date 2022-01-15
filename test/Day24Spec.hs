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
                 
  describe "initialBlackTiles" $ do
    it "testcases" $ do
      let expected = [Position (-2) 0,Position (-2) 1,Position (-1) (-2),Position (-1) (-1),Position (-1) 1,Position 0 (-3),Position 0 0,Position 0 3,Position 2 (-2),Position 2 0]
      
      initialBlackTiles (input "./input/Day24p1test.txt") `shouldBe` expected
                 
  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day24p1test.txt") `shouldBe` 10

    it "puzzle" $ do
      part1 (input "./input/Day24p1.txt") `shouldBe` 269
                 
  describe "checkRule" $ do
    it "testcases" $ do
      let tiles = initialBlackTiles (input "./input/Day24p1test.txt")
      let expected = [Position 0 3,Position 2 (-2),Position 2 0]
      
      checkRule blackRule (numberOfAdjacentBlackTiles' tiles tiles) `shouldBe` expected

  describe "allWhiteTiles" $ do
    it "simple" $ do
      let tiles = [Position 0 0]
      let expected = [Position (-1) 0,Position (-1) 1,Position 0 (-1),Position 0 1,Position 1 (-1),Position 1 0]
      
      allWhiteTiles tiles `shouldBe` expected

    it "testcases" $ do
      let tiles = initialBlackTiles (input "./input/Day24p1test.txt")
      let expected = [Position (-3) 0,Position (-3) 1,Position (-3) 2,Position (-2) (-2),Position (-2) (-1),Position (-2) 2,Position (-1) (-3),Position (-1) 0,Position (-1) 2,Position (-1) 3,Position (-1) 4,Position 0 (-4),Position 0 (-2),Position 0 (-1),Position 0 1,Position 0 2,Position 0 4,Position 1 (-4),Position 1 (-3),Position 1 (-2),Position 1 (-1),Position 1 0,Position 1 1,Position 1 2,Position 1 3,Position 2 (-3),Position 2 (-1),Position 2 1,Position 3 (-3),Position 3 (-2),Position 3 (-1),Position 3 0]
      
      allWhiteTiles tiles `shouldBe` expected

  describe "doDay" $ do
    it "testcases" $ do
      let tiles = initialBlackTiles (input "./input/Day24p1test.txt")
      length tiles `shouldBe` 10
      (length . doDay) tiles `shouldBe` 15 
      (length . doDay . doDay) tiles `shouldBe` 12 

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day24p1test.txt") `shouldBe` 2208

    it "puzzle" $ do
      part2 (input "./input/Day24p1.txt") `shouldBe` 3667
