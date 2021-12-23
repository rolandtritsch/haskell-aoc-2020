module CircularListSpec where

import CircularList
import Test.Hspec
import Control.Exception

run :: IO ()
run = hspec $ do
  describe "get" $ do
    it "simple" $ do
      let cl = CircularList 0 [1]
      get cl `shouldBe` 1

    it "empty" $ do
      let cl = CircularList 0 []
      evaluate (get cl) `shouldThrow` (== ListIsEmptyException)

  describe "forward" $ do
    it "simple" $ do
      let cl = CircularList 0 [1, 2]
      (get . forward) cl `shouldBe` 2

    it "empty" $ do
      let cl = CircularList 0 []
      forward cl `shouldBe` cl

    it "singleton" $ do
      let cl = CircularList 0 [1]
      (get . forward) cl `shouldBe` 1

  describe "backward" $ do
    it "simple" $ do
      let cl = CircularList 0 [1, 2]
      (get . backward) cl `shouldBe` 2

    it "empty" $ do
      let cl = CircularList 0 []
      backward cl `shouldBe` cl

    it "singleton" $ do
      let cl = CircularList 0 [1]
      (get . backward) cl `shouldBe` 1

  describe "insert" $ do
    it "simple" $ do
      let cl = CircularList 0 [1, 2]
      (get . insert 99) cl `shouldBe` 99

    it "singleton" $ do
      let cl = CircularList 0 [1]
      (get . insert 99) cl `shouldBe` 99
      (get . forward . insert 99) cl `shouldBe` 1
      (get . backward . insert 99) cl `shouldBe` 1
      (get . forward . insert 99) cl `shouldBe` 1
      (get . forward . forward . insert 99) cl `shouldBe` 99

    it "complex" $ do
      let cl = (insert 3 . insert 2) (CircularList 0 [1])
      get cl `shouldBe` 3
      (get . forward) cl `shouldBe` 1
      (get . forward . forward) cl `shouldBe` 2
      (get . forward . forward . forward) cl `shouldBe` 3
      (get . forward . forward . forward . forward) cl `shouldBe` 1

  describe "remove" $ do
    it "simple" $ do
      let cl = CircularList 0 [1, 2]
      (get . remove) cl `shouldBe` 2

    it "empty" $ do
      let cl = CircularList 0 []
      remove cl `shouldBe` cl

  describe "move" $ do
    it "simple" $ do
      let cl = CircularList 0 [1, 2]
      (get . move 2) cl `shouldBe` 2

    it "empty" $ do
      let cl = CircularList 0 []
      move 0 cl `shouldBe` cl

    it "not found" $ do
      let cl = CircularList 0 [1,2,3]
      move 4 cl `shouldBe` cl

    it "complex" $ do
      let cl = move 3 (CircularList 0 [1,2,3])
      get cl `shouldBe` 3
      (get .forward) cl `shouldBe` 1
