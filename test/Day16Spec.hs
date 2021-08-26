module Day16Spec where

import qualified Data.Map as M
import Data.List (transpose)

import Day16
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let program = input "./input/Day16p1test.txt"
      length (ranges program) `shouldBe` 3
      length (myTicket program) `shouldBe` 3
      length (nearbyTickets program) `shouldBe` 4 

  describe "invalidFields" $ do
    it "testcases" $ do
      invalidFields (input "./input/Day16p1test.txt") `shouldBe` [4,55,12]

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day16p1test.txt") `shouldBe` 71

    it "puzzle" $ do
      part1 (input "./input/Day16p1.txt") `shouldBe` 21071

  describe "validTickets" $ do
    it "testcases" $ do
      let notes = input "./input/Day16p1test.txt"
      validTickets (invalidFields notes) (nearbyTickets notes) `shouldBe` [[7,3,47]]
      
      let notes' = input "./input/Day16p2test.txt"
      validTickets (invalidFields notes') (nearbyTickets notes') `shouldBe` [[3,9,18],[15,1,5],[5,14,9]]

  describe "rangeIndex" $ do
    it "testcase - row" $ do
      let notes = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let fields = (M.!) (ranges notes) "row"
      rangeIndex valid fields `shouldBe` 0
      (!!) (myTicket notes) (rangeIndex valid fields) `shouldBe` 11 

    it "testcase - class" $ do
      let notes = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let fields = (M.!) (ranges notes) "class"
      rangeIndex valid fields `shouldBe` 1
      (!!) (myTicket notes) (rangeIndex valid fields) `shouldBe` 12 

    it "testcase - seat" $ do
      let notes = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let fields = (M.!) (ranges notes) "seat"
      rangeIndex valid fields `shouldBe` 2
      (!!) (myTicket notes) (rangeIndex valid fields) `shouldBe` 13 

    -- it "puzzle - departure date" $ do
    --   let notes = input "./input/Day16p1.txt"
    --   let valid = validTickets (invalidFields notes) (nearbyTickets notes)
    --   let fields = (M.!) (ranges notes) "departure date"
    --   rangeIndex valid fields `shouldBe` 14
    --   (!!) (myTicket notes) (rangeIndex valid fields) `shouldBe` 53

    -- it "puzzle - departure time" $ do
    --   let notes = input "./input/Day16p1.txt"
    --   let valid = validTickets (invalidFields notes) (nearbyTickets notes)
    --   let fields = (M.!) (ranges notes) "departure date"
    --   rangeIndex valid fields `shouldBe` 2
    --   (!!) (myTicket notes) (rangeIndex valid fields) `shouldBe` 223

  describe "check" $ do
    it "testcase" $ do
      let notes = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let fields = (M.!) (ranges notes) "seat"
      let cols = zip [0..] $ transpose valid
      (length $ filter snd $ check fields cols) `shouldBe` 1
      
    it "puzzle" $ do
      let notes = input "./input/Day16p1.txt"
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let fields = (M.!) (ranges notes) "arrival location"
      let cols = zip [0..] $ transpose valid
      (length $ filter snd $ check fields cols) `shouldBe` 1
      
  describe "find" $ do
    it "testcase - first" $ do
      let notes = input "./input/Day16p2test.txt"
      let ranges' = M.toList $ ranges notes
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let cols = zip [0..] $ transpose valid
      (find' $ check' ranges' cols) `shouldBe` ("seat", 2)
      
    it "testcase - second" $ do
      let notes = input "./input/Day16p2test.txt"
      let ranges' = M.toList $ M.delete "seat" $ ranges notes
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let cols = zip [0..] $ transpose valid
      let cols' = take 2 cols
      (find' $ check' ranges' cols') `shouldBe` ("class", 1)
      
    it "puzzle - first" $ do
      let notes = input "./input/Day16p1.txt"
      let ranges' = M.toList $ ranges notes
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let cols = zip [0..] $ transpose valid
      (find' $ check' ranges' cols) `shouldBe` ("arrival location", 9)
      
    it "puzzle - second" $ do
      let notes = input "./input/Day16p1.txt"
      let ranges' = M.toList $ M.delete "arrival location" $ ranges notes
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let cols = zip [0..] $ transpose valid
      let cols' = (take 9 cols) ++ (drop 10 cols)
      (find' $ check' ranges' cols') `shouldBe` ("class", 0)
      
  describe "collect" $ do
    it "testcase" $ do
      let notes = input "./input/Day16p2test.txt"
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let cols = zip [0..] $ transpose valid
      (collect (ranges notes) cols []) `shouldBe` [("row",0),("class",1),("seat",2)]
      
    it "puzzle" $ do
      let notes = input "./input/Day16p1.txt"
      let valid = validTickets (invalidFields notes) (nearbyTickets notes)
      let cols = zip [0..] $ transpose valid
      (collect (ranges notes) cols []) `shouldBe` [("price",4),("duration",15),("arrival track",13),("row",1),("departure platform",18),("departure track",5),("departure date",14),("departure station",8),("departure location",10),("departure time",2),("arrival platform",17),("route",12),("zone",6),("arrival station",11),("seat",19),("type",3),("wagon",7),("train",16),("class",0),("arrival location",9)]
      
  describe "solve" $ do
    it "testcase" $ do
      solve "" (input "./input/Day16p2test.txt") `shouldBe` [11,12,13]
      
    it "puzzle" $ do
      solve "departure" (input "./input/Day16p1.txt") `shouldBe` [83,227,53,73,211,223]
  describe "part2" $ do
    it "puzzle" $ do
      part2 (input "./input/Day16p1.txt") `shouldBe` 3429967441937
