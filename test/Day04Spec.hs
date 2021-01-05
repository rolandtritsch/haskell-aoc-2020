module Day04Spec where

import Day04
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let expected = Passport (Just "1937") (Just "2017") (Just "2020") (Just "183cm") (Just "#fffffd") (Just "gry") (Just "860033327") (Just "147")
      head (input "./input/Day04p1test.txt") `shouldBe` expected

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day04p1test.txt") `shouldBe` 2

    it "puzzle" $ do
      part1 (input "./input/Day04p1.txt") `shouldBe` 202

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day04p1test.txt") `shouldBe` 2

    it "puzzle" $ do
      part2 (input "./input/Day04p1.txt") `shouldBe` 139
