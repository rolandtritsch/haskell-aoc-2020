module Day23Spec where

import CircularList
import Day23
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (State _ cups _) = input "./input/Day23p1test.txt"
      size cups `shouldBe` 9

  describe "removeCups" $ do
    it "move1" $ do
      let stateBefore = State 1 (fromList 9 [3,8,9,1,2,5,4,6,7]) []
      let stateAfter = State 1 (fromList 9 [3,2,5,4,6,7]) [8,9,1]
      removeCups stateBefore `shouldBe` stateAfter

    it "move2" $ do
      let stateBefore = State 1 ((move 2 . fromList 9) [3,2,8,9,1,5,4,6,7]) []
      let stateAfter = State 1 ((move 2 . fromList 9) [3,2,5,4,6,7]) [8,9,1]
      removeCups stateBefore `shouldBe` stateAfter

    it "move3" $ do
      let stateBefore = State 1 ((move 5 . fromList 9) [3,2,5,4,6,7,8,9,1]) []
      let stateAfter = State 1 ((move 5 . forward . fromList 9) [3,2,5,8,9,1]) [4,6,7]
      removeCups stateBefore `shouldBe` stateAfter

    it "move4" $ do
      let stateBefore = State 1 ((move 8 . fromList 9) [3,4,6,7,2,5,8,9,1]) []
      let stateAfter = State 1 ((move 8 . fromList 9) [4,6,7,2,5,8]) [9,1,3]
      removeCups stateBefore `shouldBe` stateAfter
      
  describe "selectDestination" $ do
    it "move1" $ do
      let stateBefore = State 1 (fromList 9 [3,2,5,4,6,7]) [8,9,1]
      let stateAfter = State 1 ((move 2 . push . fromList 9) [3,2,5,4,6,7]) [8,9,1]
      selectDestination stateBefore `shouldBe` stateAfter

    it "move2" $ do
      let stateBefore = State 1 ((move 2 . fromList 9) [3,2,5,4,6,7]) [8,9,1]
      let stateAfter = State 1 ((move 7 . push . move 2 . fromList 9) [3,2,5,4,6,7]) [8,9,1]
      selectDestination stateBefore `shouldBe` stateAfter

    it "move3" $ do
      let stateBefore = State 1 ((move 5 . fromList 9) [3,2,5,8,9,1]) [4,6,7]
      let stateAfter = State 1 ((move 3 . push . move 5 . fromList 9) [3,2,5,8,9,1]) [4,6,7]
      selectDestination stateBefore `shouldBe` stateAfter

  describe "placePickupCups" $ do
    it "move1" $ do
      let stateBefore = State 1 ((move 2 . push . fromList 9) [3,2,5,4,6,7]) [8,9,1]
      let stateAfter = State 1 (fromList 9 [3,2,8,9,1,5,4,6,7]) []
      placePickupCups stateBefore `shouldBe` stateAfter

    it "move2" $ do
      let stateBefore = State 1 ((move 7 . push . move 2 . fromList 9) [3,2,5,4,6,7]) [8,9,1]
      let stateAfter = State 1 ((move 2 . fromList 9) [3,2,5,4,6,7,8,9,1]) []
      placePickupCups stateBefore `shouldBe` stateAfter

    it "move3" $ do
      let stateBefore = State 1 ((move 3 . push . move 5 . fromList 9) [3,2,5,8,9,1]) [4,6,7]
      let stateAfter = State 1 ((move 5 . fromList 9) [3,4,6,7,2,5,8,9,1]) []
      placePickupCups stateBefore `shouldBe` stateAfter

  describe "newCurrentCup" $ do
    it "move1" $ do
      let stateBefore = State 1 (fromList 9 [3,2,8,9,1,5,4,6,7]) []
      let stateAfter = State 0 ((move 2 . fromList 9) [3,2,8,9,1,5,4,6,7]) []
      newCurrentCup stateBefore `shouldBe` stateAfter

    it "move2" $ do
      let stateBefore = State 1 ((move 2 . fromList 9) [3,2,5,4,6,7,8,9,1]) []
      let stateAfter = State 0 ((move 5 . fromList 9) [3,2,5,4,6,7,8,9,1]) []
      newCurrentCup stateBefore `shouldBe` stateAfter

    it "move3" $ do
      let stateBefore = State 1 ((move 5 . fromList 9) [3,4,6,7,2,5,8,9,1]) []
      let stateAfter = State 0 ((move 8 . fromList 9) [3,4,6,7,2,5,8,9,1]) []
      newCurrentCup stateBefore `shouldBe` stateAfter

  describe "actions" $ do
    it "move1" $ do
      let stateBefore = State 1 (fromList 9 [3,8,9,1,2,5,4,6,7]) []
      let stateAfter = State 0 ((move 2 . fromList 9) [3,2,8,9,1,5,4,6,7]) []
      actions stateBefore `shouldBe` stateAfter

    it "move2" $ do
      let stateBefore = State 1 ((move 2 . fromList 9) [3,2,8,9,1,5,4,6,7]) []
      let stateAfter = State 0 ((move 5 . fromList 9) [3,2,5,4,6,7,8,9,1]) []
      actions stateBefore `shouldBe` stateAfter

    it "move3" $ do
      let stateBefore = State 1 ((move 5 . fromList 9) [3,2,5,4,6,7,8,9,1]) []
      let stateAfter = State 0 ((move 8 . fromList 9) [3,4,6,7,2,5,8,9,1]) []
      actions stateBefore `shouldBe` stateAfter

    it "move4" $ do
      let stateBefore = State 1 ((move 8 . fromList 9) [3,4,6,7,2,5,8,9,1]) []
      let stateAfter = State 0 (fromList 9 [4,6,7,9,1,3,2,5,8]) []
      actions stateBefore `shouldBe` stateAfter

  describe "executeMoves" $ do
    it "10" $ do
      let stateBefore = State 10 (fromList 9 [3,8,9,1,2,5,4,6,7]) []
      let stateAfter = State 0 ((move 8 . fromList 9) [5,8,3,7,4,1,9,2,6]) []
      executeMoves stateBefore `shouldBe` stateAfter

  describe "collect" $ do
    it "simple" $ do
      collect 1 ((move 8 . fromList 9) [5,8,3,7,4,1,9,2,6]) `shouldBe` [9,2,6,5,8,3,7,4]

  describe "ints2Int" $ do
    it "simple" $ do
      ints2Int [1,2,3] `shouldBe` 123

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day23p1test.txt") `shouldBe` 92658374
      part1 (input "./input/Day23p1test2.txt") `shouldBe` 67384529

    it "puzzle" $ do
      part1 (input "./input/Day23p1.txt") `shouldBe` 39564287

  describe "collect'" $ do
    it "simple" $ do
       collect' 1 ((move 8 . fromList 9) [5,8,3,7,4,1,9,2,6]) `shouldBe` (9,2)

  describe "part2" $ do
    it "testcases" $ do
      -- part2 (input "./input/Day23p1test.txt") `shouldBe` 149245887792
      part2 (input "./input/Day23p1test.txt") `shouldBe` 430728

    -- it "puzzle" $ do
    --   part2 (input "./input/Day23p1.txt") `shouldBe` 9
