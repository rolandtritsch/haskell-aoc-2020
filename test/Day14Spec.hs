module Day14Spec where

import Data.Bits ((.&.), (.|.))

import Day14
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      length (input "./input/Day14p1test.txt") `shouldBe` 4

  describe "bin2dec" $ do
    it "testcases" $ do
      bin2dec "000000000000000000000000000001000000" `shouldBe` 64
      bin2dec "111111111111111111111111111111111101" `shouldBe` 68719476733

  describe "mask" $ do
    it "testcases" $ do
      let setMask = bin2dec "000000000000000000000000000001000000"
      let unsetMask = bin2dec "111111111111111111111111111111111101"
      unsetMask .&. (11 .|. setMask) `shouldBe` 73
      unsetMask .&. (101 .|. setMask) `shouldBe` 101
      unsetMask .&. (0 .|. setMask) `shouldBe` 64

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day14p1test.txt") `shouldBe` 165

    it "puzzle" $ do
      part1 (input "./input/Day14p1.txt") `shouldBe` 7440382076205

  describe "buildMasks" $ do
    it "testcases" $ do
      let expected =
            [
              "1000",
              "1001",
              "1010",
              "1011"
            ]              
      buildMasks "10XX" `shouldBe` expected

  -- describe "part2" $ do
  --   it "testcases" $ do
  --     part2 (input "./input/Day14p1test.txt") `shouldBe` 208

  --   it "puzzle" $ do
  --     part2 (input "./input/Day14p1.txt") `shouldBe` 0
