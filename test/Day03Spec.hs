module Day03Spec where

import Day03
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      input "./input/Day03p1test.txt" `shouldBe` (Forrest 10 10 [(0,2),(0,3),(1,0),(1,4),(1,8),(2,1),(2,6),(2,9),(3,2),(3,4),(3,8),(3,10),(4,1),(4,5),(4,6),(4,9),(5,2),(5,4),(5,5),(6,1),(6,3),(6,5),(6,10),(7,1),(7,10),(8,0),(8,2),(8,3),(8,7),(9,0),(9,4),(9,5),(9,10),(10,1),(10,4),(10,8),(10,10)])

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day03p1test.txt") (3,1) `shouldBe` 7
      
    it "puzzle" $ do
      part1 (input "./input/Day03p1.txt") (3,1) `shouldBe` 195

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day03p1test.txt") `shouldBe` 336

    it "puzzle" $ do
      part2 (input "./input/Day03p1.txt") `shouldBe` 3772314000
