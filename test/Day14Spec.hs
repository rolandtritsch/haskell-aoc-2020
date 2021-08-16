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

  describe "dec2bin" $ do
    it "testcases" $ do
      dec2bin 42 `shouldBe` "101010" 
      dec2bin 26 `shouldBe` "11010" 

  describe "prepend" $ do
    it "testcases" $ do
      prepend '0' 4 "10" `shouldBe` "0010" 

  describe "applyMask" $ do
    it "testcases" $ do
      applyMask "101010" "X1001X" `shouldBe` "X1101X" 
      applyMask "11010" "0X0XX" `shouldBe` "1X0XX" 

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

      let expected' =
            [
              "010010",
              "010011",
              "110010",
              "110011"
            ]              
      buildMasks "X1001X" `shouldBe` expected'

  describe "memMasks" $ do
    it "testcases" $ do
      let expected = [26,27,58,59]
      let as = map bin2dec $ buildMasks $ applyMask (dec2bin 42) "X1001X"
      as `shouldBe` expected

      let expected' = [16,17,18,19,24,25,26,27]
      let as' = map bin2dec $ buildMasks $ applyMask (dec2bin 26) "0X0XX"
      as' `shouldBe` expected'

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day14p2test.txt") `shouldBe` 208

    it "puzzle" $ do
      part2 (input "./input/Day14p1.txt") `shouldBe` 4200656704538
