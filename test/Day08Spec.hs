module Day08Spec where

import Day08
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      head (input "./input/Day08p1test.txt") `shouldBe` (Instruction NOP 0)

  describe "runProgram" $ do
    it "LOOP" $ do
      runProgram (input "./input/Day08p1test.txt") (0,0) [] `shouldBe` (5, LOOP)

    it "NORMAL" $ do
      runProgram (input "./input/Day08p1fixed.txt") (0,0) [] `shouldBe` (8, NORMAL)

  describe "buildInstructions" $ do
    it "testcases" $ do
      let expected = [[Instruction JMP 0,Instruction ACC 1,Instruction JMP 4,Instruction ACC 3,Instruction JMP (-3),Instruction ACC (-99),Instruction ACC 1,Instruction JMP (-4),Instruction ACC 6],[Instruction NOP 0,Instruction ACC 1,Instruction JMP 4,Instruction ACC 3,Instruction JMP (-3),Instruction ACC (-99),Instruction ACC 1,Instruction JMP (-4),Instruction ACC 6],[Instruction NOP 0,Instruction ACC 1,Instruction NOP 4,Instruction ACC 3,Instruction JMP (-3),Instruction ACC (-99),Instruction ACC 1,Instruction JMP (-4),Instruction ACC 6],[Instruction NOP 0,Instruction ACC 1,Instruction JMP 4,Instruction ACC 3,Instruction NOP (-3),Instruction ACC (-99),Instruction ACC 1,Instruction JMP (-4),Instruction ACC 6],[Instruction NOP 0,Instruction ACC 1,Instruction JMP 4,Instruction ACC 3,Instruction JMP (-3),Instruction ACC (-99),Instruction ACC 1,Instruction NOP (-4),Instruction ACC 6]]                            
      buildInstructions (input "./input/Day08p1test.txt") `shouldBe` expected

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day08p1test.txt") `shouldBe` 5

    it "puzzle" $ do
      part1 (input "./input/Day08p1.txt") `shouldBe` 1563

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day08p1test.txt") `shouldBe` 8

    it "puzzle" $ do
      part2 (input "./input/Day08p1.txt") `shouldBe` 767
