module Day16Spec where

import qualified Data.Map as M
import Data.List (transpose)

import Day16
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (Notes ranges myTicket nearbyTickets) = input "./input/Day16p1test.txt"
      M.size ranges `shouldBe` 3
      length myTicket `shouldBe` 3
      length nearbyTickets `shouldBe` 4 

  describe "invalidFields" $ do
    it "testcases" $ do
      invalidFields (input "./input/Day16p1test.txt") `shouldBe` [4,55,12]

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day16p1test.txt") `shouldBe` 71

    it "puzzle" $ do
      part1 (input "./input/Day16p1.txt") `shouldBe` 21071

  describe "validTickets" $ do
    it "testcases - p1" $ do
      let notes@(Notes _ _ nearbyTickets) = input "./input/Day16p1test.txt"
      validTickets (invalidFields notes) nearbyTickets `shouldBe` [[7,3,47]]
      
    it "testcases - p2" $ do
      let notes@(Notes _ _ nearbyTickets) = input "./input/Day16p2test.txt"
      validTickets (invalidFields notes) nearbyTickets `shouldBe` [[3,9,18],[15,1,5],[5,14,9]]
      
    it "puzzle" $ do
      let notes@(Notes _ _ nearbyTickets) = input "./input/Day16p1.txt"
      (length $ validTickets (invalidFields notes) nearbyTickets) `shouldBe` 190

  describe "check" $ do
    it "testcase" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      let fields = (M.!) ranges "seat"
      (length $ filter snd $ check fields cols) `shouldBe` 1
      
    it "puzzle" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p1.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      let fields = (M.!) ranges "arrival location"
      (length $ filter snd $ check fields cols) `shouldBe` 1
      
  describe "unique" $ do
    it "testcase - first" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      (unique $ checks ranges cols) `shouldBe` ("seat", 2)

    it "testcase - second" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      let ranges' = M.delete "seat" ranges
      let cols' = M.delete 2 cols
      (unique $ checks ranges' cols') `shouldBe` ("class", 1)
      
    it "puzzle - first" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p1.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      (unique $ checks ranges cols) `shouldBe` ("arrival location", 9)
      
    it "puzzle - second" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p1.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      let ranges' = M.delete "arrival location" ranges
      let cols' = M.delete 9 cols
      (unique $ checks ranges' cols') `shouldBe` ("class", 0)
      
  describe "collect" $ do
    it "testcase" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      (M.toList $ collect ranges cols M.empty) `shouldBe` [("class",1),("row",0),("seat",2)]
      
    it "puzzle" $ do
      let notes@(Notes ranges _ nearbyTickets) = input "./input/Day16p1.txt"
      let valid = validTickets (invalidFields notes) nearbyTickets
      let cols = M.fromList $ zip [0..] $ transpose valid
      (M.toList $ collect ranges cols M.empty) `shouldBe` [("arrival location",9),("arrival platform",17),("arrival station",11),("arrival track",13),("class",0),("departure date",14),("departure location",10),("departure platform",18),("departure station",8),("departure time",2),("departure track",5),("duration",15),("price",4),("route",12),("row",1),("seat",19),("train",16),("type",3),("wagon",7),("zone",6)]
      
  describe "solve" $ do
    it "testcase" $ do
      solve "" (input "./input/Day16p2test.txt") `shouldBe` [12,11,13]
      
    it "puzzle" $ do
      solve "departure" (input "./input/Day16p1.txt") `shouldBe` [53,211,83,73,223,227]
  describe "part2" $ do
    it "puzzle" $ do
      part2 (input "./input/Day16p1.txt") `shouldBe` 3429967441937
