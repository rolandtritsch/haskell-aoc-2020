module CircularListSpec where

import CircularList
import Test.Hspec
import Control.Exception

run :: IO ()
run = hspec $ do
  describe "lengthCL" $ do
    it "simple" $ do
      let cl = CircularList 0 [1] []
      lengthCL cl `shouldBe` 1

    it "empty" $ do
      let cl = CircularList 0 [] []
      lengthCL cl `shouldBe` 0

  describe "isEmpty" $ do
    it "simple" $ do
      let cl = CircularList 0 [] []
      isEmpty cl `shouldBe` True
      (isEmpty . insert 1) cl `shouldBe` False

  describe "isIn" $ do
    it "simple" $ do
      let cl = CircularList 0 [1] []
      isIn 1 cl `shouldBe` True
      isIn 99 cl `shouldBe` False

    it "empty" $ do
      let cl = CircularList 0 [] []
      isIn 99 cl `shouldBe` False
      
  describe "get" $ do
    it "simple" $ do
      let cl = CircularList 0 [1] []
      get cl `shouldBe` 1

    it "empty" $ do
      let cl = CircularList 0 [] []
      evaluate (get cl) `shouldThrow` (== ListIsEmptyException)

  describe "forward" $ do
    it "simple" $ do
      let cl = CircularList 0 [1,2] []
      (get . forward) cl `shouldBe` 2

    it "empty" $ do
      let cl = CircularList 0 [] []
      evaluate (forward cl) `shouldThrow` (== ListIsEmptyException)

    it "singleton" $ do
      let cl = CircularList 0 [1] []
      (get . forward) cl `shouldBe` 1

  describe "backward" $ do
    it "simple" $ do
      let cl = CircularList 0 [1,2] []
      (get . backward) cl `shouldBe` 2

    it "empty" $ do
      let cl = CircularList 0 [] []
      evaluate (backward cl) `shouldThrow` (== ListIsEmptyException)

    it "singleton" $ do
      let cl = CircularList 0 [1] []
      (get . backward) cl `shouldBe` 1

  describe "insert" $ do
    it "simple" $ do
      let cl = CircularList 0 [1,2] []
      (get . insert 99) cl `shouldBe` 1
      (get . forward . insert 99) cl `shouldBe` 99

    it "singleton" $ do
      let cl = CircularList 0 [1] []
      (get . insert 99) cl `shouldBe` 1
      (get . forward . insert 99) cl `shouldBe` 99
      (get . backward . insert 99) cl `shouldBe` 99
      (get . forward . forward . insert 99) cl `shouldBe` 1

    it "complex" $ do
      let cl = (insert 2 . insert 3) (CircularList 0 [1] [])
      get cl `shouldBe` 1
      (get . forward) cl `shouldBe` 2
      (get . forward . forward) cl `shouldBe` 3
      (get . forward . forward . forward) cl `shouldBe` 1

    it "first" $ do
      let cl = CircularList 0 [1,2,3] []
      insert 99 cl `shouldBe` (CircularList 0 [1,99,2,3] [])

    it "last" $ do
      let cl = CircularList 2 [1,2,3] []
      insert 99 cl `shouldBe` (CircularList 2 [1,2,3,99] [])

  describe "remove" $ do
    it "simple" $ do
      let cl = CircularList 0 [1,2,3] []
      remove cl `shouldBe` CircularList 0 [1,3] []

    it "empty" $ do
      let cl = CircularList 0 [] []
      evaluate (remove cl) `shouldThrow` (== ListIsEmptyException)

    it "last" $ do
      let cl = CircularList 2 [1,2,3] []
      remove cl `shouldBe` (CircularList 1 [2,3] [])

  describe "move" $ do
    it "simple" $ do
      let cl = CircularList 0 [1,2] []
      (get . move 2) cl `shouldBe` 2

    it "empty" $ do
      let cl = CircularList 0 [] []
      evaluate (move 0 cl) `shouldThrow` (== ListIsEmptyException)

    it "not found" $ do
      let cl = CircularList 0 [1,2,3] []
      evaluate (move 99 cl) `shouldThrow` (== ItemNotFoundException)

    it "complex" $ do
      let cl = move 3 (CircularList 0 [1,2,3] [])
      get cl `shouldBe` 3
      (get .forward) cl `shouldBe` 1

  describe "toList" $ do
    it "simple" $ do
      let cl = CircularList 0 [1,2,3] []
      (toList . move 2) cl `shouldBe` [2,3,1]

    it "empty" $ do
      let cl = CircularList 0 [] []
      toList cl `shouldBe` []

  describe "push" $ do
    it "simple" $ do
      let cl = CircularList 0 [1,2,3] []
      push cl `shouldBe` (CircularList 0 [1,2,3] [1])

    it "empty" $ do
      let cl = CircularList 0 [] []
      evaluate (push cl) `shouldThrow` (== ListIsEmptyException)

  describe "pop" $ do
    it "simple" $ do
      let cl = CircularList 2 [1,2,3] [1]
      pop cl `shouldBe` (CircularList 0 [1,2,3] [])

    it "empty" $ do
      let cl = CircularList 0 [] [1]
      evaluate (pop cl) `shouldThrow` (== ListIsEmptyException)

    it "empty stack" $ do
      let cl = CircularList 0 [] []
      evaluate (pop cl) `shouldThrow` (== StackIsEmptyException)
