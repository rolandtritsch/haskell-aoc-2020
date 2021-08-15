module Day13Spec where

import Day13
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let schedule = input "./input/Day13p1test.txt"
      departure schedule `shouldBe` 939
      length (busses schedule) `shouldBe` 5
      (busses schedule) `shouldBe` [7,13,59,31,19]
      (busses' schedule) `shouldBe` [(7,7),(13,12),(59,55),(31,25),(19,12)]

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day13p1test.txt") `shouldBe` 295

    it "puzzle" $ do
      part1 (input "./input/Day13p1.txt") `shouldBe` 2305

  describe "part2'" $ do
    it "testcases" $ do
      part2' (Schedule {departure = 0, busses = [], busses' = [(17,17),(13,11),(19,16)]}) `shouldBe` 3417
      part2' (Schedule {departure = 0, busses = [], busses' = [(67, 67),(7,6),(59,57),(61,58)]}) `shouldBe` 754018
      part2' (Schedule {departure = 0, busses = [], busses' = [(67,67),(7,5),(59,56),(61,57)]}) `shouldBe` 779210
      part2' (Schedule {departure = 0, busses = [], busses' = [(67,67),(7,6),(59,56),(61,57)]}) `shouldBe` 1261476
      part2' (Schedule {departure = 0, busses = [], busses' = [(1789,1789),(37,36),(47,45),(1889,1886)]}) `shouldBe` 1202161486 
      part2' (input "./input/Day13p1test.txt") `shouldBe` 1068781
      
  describe "part2" $ do
    it "testcases" $ do
      part2 (Schedule {departure = 0, busses = [], busses' = [(17,17),(13,11),(19,16)]}) `shouldBe` 3417
      part2 (Schedule {departure = 0, busses = [], busses' = [(67,67),(7,6),(59,57),(61,58)]}) `shouldBe` 754018
      part2 (Schedule {departure = 0, busses = [], busses' = [(67,67),(7,5),(59,56),(61,57)]}) `shouldBe` 779210
      part2 (Schedule {departure = 0, busses = [], busses' = [(67,67),(7,6),(59,56),(61,57)]}) `shouldBe` 1261476
      part2 (Schedule {departure = 0, busses = [], busses' = [(1789,1789),(37,36),(47,45),(1889,1886)]}) `shouldBe` 1202161486 
      part2 (input "./input/Day13p1test.txt") `shouldBe` 1068781
      
    it "puzzle" $ do
      part2 (input "./input/Day13p1.txt") `shouldBe` 552612234243498
      
