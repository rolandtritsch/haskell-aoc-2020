module Day15Spec where

import Day15
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day15p1test.txt") `shouldBe` 3

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day15p1test.txt") `shouldBe` 436

    it "puzzle" $ do
      part1 (input "./input/Day15p1.txt") `shouldBe` 232

  describe "part2" $ do
    -- it "testcases" $ do
    --   part2 [1,3,2] `shouldBe` 2578
    --   part2 [2,1,3] `shouldBe` 3544142
    --   part2 [1,2,3] `shouldBe` 261214
    --   part2 [2,3,1] `shouldBe` 6895259
    --   part2 [3,2,1] `shouldBe` 18
    --   part2 [3,1,2] `shouldBe` 362
    --   part2 (input "./input/Day15p1test.txt") `shouldBe` 175594

    it "puzzle" $ do
      part2 (input "./input/Day15p1.txt") `shouldBe` 18929178
