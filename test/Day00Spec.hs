module Day00Spec where

import Text.Megaparsec (parse)
--import Text.Megaparsec.Debug (dbg)
import Test.Hspec.Megaparsec (shouldParse, parseSatisfies)

import Test.Hspec

import Day00

import Util (inputRaw)

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head input `shouldBe` "Hello"
      last input `shouldBe` "World"

  describe "parse" $ do
    it "parse" $ do
      parse parseWord "" "Roland" `shouldParse` "Roland"
      parse parseWords "" (inputRaw "./input/Day00p1.txt") `parseSatisfies` ((==) 2 . length)

  describe "part1" $ do
    it "testcases" $ do
      part1 [] `shouldBe` 1

    it "puzzle" $ do
      part1 input `shouldBe` 1

  describe "part2" $ do
    it "testcases" $ do
      part2 [] `shouldBe` 2

    it "puzzle" $ do
      part2 input `shouldBe` 2
