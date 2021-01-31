{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day21Spec where

import Day21
import Test.Hspec

import qualified Data.Map as M

run :: IO ()
run = hspec $ do
  let input' = input "./input/Day21p1test.txt"
  
  describe "input" $ do
    it "input" $ do
      M.size input' `shouldBe` 3

  describe "intersect" $ do
    it "should work" $ do
      intersect [1,2] [2,3] `shouldBe` [2]
      intersect' [[1,2],[2,3]] `shouldBe` [2]

    it "should work with empty sets" $ do
      intersect [1,2] [] `shouldBe` [] 
      intersect' [[1,2],[]] `shouldBe` []
      intersect [] [1,2] `shouldBe` [] 
      intersect' [[],[1,2]] `shouldBe` []
      intersect [3,4] [1,2] `shouldBe` [] 
      intersect' [[3,4],[1,2]] `shouldBe` []

  describe "diffr" $ do
    it "should work" $ do
      diffr [1,2] [2,3] `shouldBe` [3]
      diffr [1,4] [2,3] `shouldBe` [2,3]
      diffr [2,3] [2,3] `shouldBe` []
      diffr [] [2,3] `shouldBe` [2,3]
      diffr [1,2] [] `shouldBe` []

  describe "removeIngredientsByIntersection" $ do
    it "should work" $ do
      let expected = [
            ("dairy",[["trh","fvjkl","sbzzf"],["kfcds","sqjhc","nhms"]]),
            ("fish",[["sqjhc","sbzzf"],["kfcds","sqjhc","nhms"]]),
            ("soy",[["sqjhc","fvjkl"]])
            ]
      M.toList (removeIngredientsByIntersection input') `shouldBe` expected
      
  describe "removeIngredientsBySingleton" $ do
    it "should work" $ do
      let input'' = [
            ("dairy",[["trh","fvjkl","sbzzf","mxmxvkd"],["mxmxvkd","kfcds","sqjhc","nhms"]]),
            ("fish",[["sqjhc","mxmxvkd","sbzzf"],["mxmxvkd","kfcds","sqjhc","nhms"]]),
            ("soy",[["sqjhc"]])
            ]
      let expected = [
            ("dairy",[["trh","fvjkl","sbzzf","mxmxvkd"],["mxmxvkd","kfcds","nhms"]]),
            ("fish",[["mxmxvkd","sbzzf"],["mxmxvkd","kfcds","nhms"]]),
            ("soy",[[]])
            ]
      M.toList (removeIngredientsBySingleton (M.fromList input'')) `shouldBe` expected
      
  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day21p1test.txt") `shouldBe` 5 

    it "puzzle" $ do
      part1 (input "./input/Day21p1.txt") `shouldBe` 2584

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day21p1test.txt") `shouldBe` 3

    it "puzzle" $ do
      part2 (input "./input/Day21p1.txt") `shouldBe` 8
