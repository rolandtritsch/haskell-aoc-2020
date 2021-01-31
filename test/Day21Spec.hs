{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day21Spec where

import Day21
import Test.Hspec

import qualified Data.Map as M
import qualified Data.Set as S

run :: IO ()
run = hspec $ do
  let input' = input "./input/Day21p1test.txt"
  
  describe "input" $ do
    it "input" $ do
      M.size input' `shouldBe` 3

  describe "intersect" $ do
    it "should work" $ do
      intersect (S.fromList [1,2]) (S.fromList [2,3]) `shouldBe` (S.fromList [2])
      intersect' [(S.fromList [1,2]),(S.fromList [2,3])] `shouldBe` (S.fromList [2])

    it "should work with empty sets" $ do
      intersect (S.fromList [1,2]) (S.fromList []) `shouldBe` (S.fromList [])
      intersect' [(S.fromList [1,2]),(S.fromList [])] `shouldBe` (S.fromList [])
      intersect (S.fromList []) (S.fromList [1,2]) `shouldBe` (S.fromList [])
      intersect' [(S.fromList []),(S.fromList [1,2])] `shouldBe` (S.fromList [])
      intersect (S.fromList [3,4]) (S.fromList [1,2]) `shouldBe` (S.fromList [])
      intersect' [(S.fromList [3,4]),(S.fromList [1,2])] `shouldBe` (S.fromList [])

  describe "diffr" $ do
    it "should work" $ do
      diffr (S.fromList [1,2]) (S.fromList [2,3]) `shouldBe` (S.fromList [3])
      diffr (S.fromList [1,4]) (S.fromList [2,3]) `shouldBe` (S.fromList [2,3])
      diffr (S.fromList [2,3]) (S.fromList [2,3]) `shouldBe` (S.fromList [])
      diffr (S.fromList []) (S.fromList [2,3]) `shouldBe` (S.fromList [2,3])
      diffr (S.fromList [1,2]) (S.fromList []) `shouldBe` (S.fromList [])

  describe "removeIngredientsByIntersection" $ do
    it "should work" $ do
      let expected = [
            ("dairy",[S.fromList ["trh","fvjkl","sbzzf"],S.fromList ["kfcds","sqjhc","nhms"]]),
            ("fish",[S.fromList ["sqjhc","sbzzf"],S.fromList ["kfcds","sqjhc","nhms"]]),
            ("soy",[S.fromList ["sqjhc","fvjkl"]])
            ]
      M.toList (removeIngredientsByIntersection input') `shouldBe` expected
      
  describe "removeIngredientsBySingleton" $ do
    it "should work" $ do
      let input'' = [
            ("dairy",[S.fromList ["trh","fvjkl","sbzzf","mxmxvkd"],S.fromList ["mxmxvkd","kfcds","sqjhc","nhms"]]),
            ("fish",[S.fromList ["sqjhc","mxmxvkd","sbzzf"],S.fromList ["mxmxvkd","kfcds","sqjhc","nhms"]]),
            ("soy",[S.fromList ["sqjhc"]])
            ]
      let expected = [
            ("dairy",[S.fromList ["trh","fvjkl","sbzzf","mxmxvkd"],S.fromList ["mxmxvkd","kfcds","nhms"]]),
            ("fish",[S.fromList ["mxmxvkd","sbzzf"],S.fromList ["mxmxvkd","kfcds","nhms"]]),
            ("soy",[S.fromList []])
            ]
      M.toList (removeIngredientsBySingleton (M.fromList input'')) `shouldBe` expected
      
  describe "part1" $ do
    it "testcases" $ do
      --part1 (input "./input/Day21p1test.txt") `shouldBe` 5 
      part1 (input "./input/Day21p1test.txt") `shouldBe` 2 

    it "puzzle" $ do
      part1 (input "./input/Day21p1.txt") `shouldBe` 2584

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day21p1test.txt") `shouldBe` 3

    it "puzzle" $ do
      part2 (input "./input/Day21p1.txt") `shouldBe` 8
