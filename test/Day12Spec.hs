module Day12Spec where

import Day12
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head (input "./input/Day12p1test.txt") `shouldBe` (Instruction Forward' 10)

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day12p1test.txt") `shouldBe` 25

    it "puzzle" $ do
      part1 (input "./input/Day12p1.txt") `shouldBe` 2297

  describe "execute'" $ do
    it "testcases" $ do
      let (i:is) = input "./input/Day12p1test.txt"
      let s = execute' ((0, 0), (10, 1)) i
      s `shouldBe` ((100, 10), (10, 1))
      
      let (i':is') = is
      let s' = execute' s i'
      s' `shouldBe` ((100, 10), (10, 4))
      
      let (i'':is'') = is'
      let s'' = execute' s' i''
      s'' `shouldBe` ((170, 38), (10, 4))
      
      let (i''':is''') = is''
      let s''' = execute' s'' i'''
      s''' `shouldBe` ((170, 38), (4, -10))
      
      let (i'''':_) = is'''
      let s'''' = execute' s''' i''''
      s'''' `shouldBe` ((214, -72), (4, -10))
      
  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day12p1test.txt") `shouldBe` 286

    it "puzzle" $ do
      part2 (input "./input/Day12p1.txt") `shouldBe` 89984
