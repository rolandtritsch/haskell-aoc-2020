module Day21Spec where

import Day21
import Test.Hspec

import qualified Data.Map as M

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let foods = input "./input/Day21p1test.txt"
      let expected =
            [
              ("dairy",[["trh","fvjkl","sbzzf","mxmxvkd"],["mxmxvkd","kfcds","sqjhc","nhms"]]),
              ("fish",[["sqjhc","mxmxvkd","sbzzf"],["mxmxvkd","kfcds","sqjhc","nhms"]]),
              ("soy",[["sqjhc","fvjkl"]])
            ]
      M.size foods `shouldBe` 3
      M.toList foods `shouldBe` expected

  describe "foodsFreeOfAllergens" $ do
    it "testcases" $ do
      let foods = input "./input/Day21p1test.txt"
      let expected =
            [
              ("dairy",[["trh","sbzzf"],["kfcds","nhms"]]),
              ("fish",[["sbzzf"],["kfcds","nhms"]]),
              ("soy",[[]])
            ]                                              
      M.toList (foodsFreeOfAllergens foods) `shouldBe` expected

    it "puzzle" $ do
      let foods = input "./input/Day21p1.txt"
      let expected = ["dairy","eggs","fish","nuts","peanuts","sesame","soy","wheat"]
      M.keys (foodsFreeOfAllergens foods) `shouldBe` expected

  describe "allIncredientsFreeOfAllergens" $ do
    it "testcases" $ do
      let foods = input "./input/Day21p1test.txt"
      let expected = ["trh","sbzzf","kfcds","nhms"]
      allIncredientsFreeOfAllergens foods `shouldBe` expected
      
  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day21p1test.txt") `shouldBe` 5 

    it "puzzle" $ do
      part1 (input "./input/Day21p1.txt") `shouldBe` 2584

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day21p1test.txt") `shouldBe` "mxmxvkd,sqjhc,fvjkl"

    it "puzzle" $ do
      part2 (input "./input/Day21p1.txt") `shouldBe` "fqhpsl,zxncg,clzpsl,zbbnj,jkgbvlxh,dzqc,ppj,glzb"
