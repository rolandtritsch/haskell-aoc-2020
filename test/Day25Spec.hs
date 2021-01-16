module Day25Spec where

import Day25
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      (input "./input/Day25p1test.txt") `shouldBe` (5764801, 17807724)

  describe "transform" $ do
    it "basic" $ do
      (transform 10 1 123) `shouldBe` 17410834
    it "door public key" $ do
      (transform 8 1 17807724) `shouldBe` 14897079
    it "card public key" $ do
      (transform 11 1 5764801) `shouldBe` 14897079
      
  describe "brute" $ do
    it "card" $ do
      (brute 1 1 5764801) `shouldBe` 8
    it "door" $ do
      (brute 1 1 17807724) `shouldBe` 11

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day25p1test.txt") `shouldBe` 14897079

    it "puzzle" $ do
      part1 (input "./input/Day25p1.txt") `shouldBe` 296776

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day25p1test.txt") `shouldBe` 5764801

    it "puzzle" $ do
      part2 (input "./input/Day25p1.txt") `shouldBe` 12578151
