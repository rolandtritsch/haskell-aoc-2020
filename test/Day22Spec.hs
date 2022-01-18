module Day22Spec where

import Day22
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (Game player1 _) = input "./input/Day22p1test.txt"
      length player1 `shouldBe` 5

  describe "playRound" $ do
    it "testcases" $ do
      let game = input "./input/Day22p1test.txt"
      let expected = Game [2,6,3,1,9,5] [8,4,7,10]
      playRound game `shouldBe` expected 

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day22p1test.txt") `shouldBe` 306

    it "puzzle" $ do
      part1 (input "./input/Day22p1.txt") `shouldBe` 32489

  describe "playRound2" $ do
    it "testcases - round1" $ do
      let game = input "./input/Day22p1test.txt"
      let expected = Game [2,6,3,1,9,5] [8,4,7,10]
      playRound2 game `shouldBe` expected

    it "testcases - round8" $ do
      let game = input "./input/Day22p1test.txt"
      let expected = Game [4,9,8,5,2] [3,10,1,7,6]
      let result = foldl (\g _ -> playRound2 g) game [1..8 :: Int]
      result `shouldBe` expected

    it "testcases - round9 (playing Game2)" $ do
      let game = input "./input/Day22p1test.txt"
      let expected = Game [9,8,5,2] [10,1,7,6,3,4]
      let result = foldl (\g _ -> playRound2 g) game [1..9 :: Int]
      result `shouldBe` expected

  describe "playGame2" $ do
    it "testcases - detect loop" $ do
      let game = input "./input/Day22p2test.txt"
      let expected = Game [43,19,2,29,14] []
      let (_, result) = playGame2 [] game
      result `shouldBe` expected

    it "testcases - game2" $ do
      let game = Game [9,8,5,2] [10,1,7,6]
      let expected = Game [] [7,5,6,2,10,8,9,1]
      let (_, result) = playGame2 [] game
      result `shouldBe` expected

  describe "playSubGame2" $ do
    it "testcases - game2" $ do
      let game = Game [4,9,8,5,2] [3,10,1,7,6]
      let expected = Game [9,8,5,2] [10,1,7,6,3,4]
      let result = playSubGame2 game
      result `shouldBe` expected

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day22p1test.txt") `shouldBe` 291

    -- it "puzzle" $ do
    --   part2 (input "./input/Day22p1.txt") `shouldBe` 35676
