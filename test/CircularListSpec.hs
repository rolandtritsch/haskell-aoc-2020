module CircularListSpec where

import CircularList
import Test.Hspec
import Control.Exception

run :: IO ()
run = hspec $ do
  describe "fromList" $ do
    it "simple" $ do
      let cl = fromList 3 [3,2,1]
      toList cl `shouldBe` [3,2,1]

    it "empty" $ do
      let cl = fromList 0 []
      toList cl `shouldBe` []

    it "empty (of size N)" $ do
      let cl = fromList 3 []
      toList cl `shouldBe` []

    it "singleton" $ do
      let cl = fromList 1 [1]
      toList cl `shouldBe` [1]
      
    it "too large" $ do
      let cl = fromList 3 [1,2,3,4]
      evaluate (toList cl) `shouldThrow` anyErrorCall
      
  describe "toList" $ do
    it "simple" $ do
      let cl = fromList 3 [1,2,3]
      toList cl `shouldBe` [1,2,3]

    it "empty" $ do
      let cl = fromList 0 []
      toList cl `shouldBe` []

    it "singleton" $ do
      let cl = fromList 1 [1]
      toList cl `shouldBe` [1]
      
  describe "size" $ do
    it "simple" $ do
      let cl = fromList 3 [1,2,3]
      size cl `shouldBe` 3

    it "empty" $ do
      let cl = fromList 0 []
      size cl `shouldBe` 0

    it "singleton" $ do
      let cl = fromList 1 [1]
      size cl `shouldBe` 1

    it "size N" $ do
      let cl = fromList 10 []
      size cl `shouldBe` 0
      
  describe "isEmpty" $ do
    it "simple" $ do
      let cl = fromList 0 []
      isEmpty cl `shouldBe` True

    it "singleton" $ do
      let cl = fromList 1 [1]
      isEmpty cl `shouldBe` False
      
    it "size N" $ do
      let cl = fromList 10 []
      isEmpty cl `shouldBe` True
            
    it "after last remove" $ do
      let cl = fromList 1 [1]
      isEmpty cl `shouldBe` False
      (isEmpty . remove) cl `shouldBe` True
                  
    it "after first insert" $ do
      let cl = fromList 1 []
      isEmpty cl `shouldBe` True
      (isEmpty . insert 1) cl `shouldBe` False
      
  describe "isIn" $ do
    it "simple" $ do
      let cl = fromList 1 [1]
      isIn 1 cl `shouldBe` True
      isIn 2 cl `shouldBe` False

    it "empty" $ do
      let cl = fromList 0 []
      isIn 99 cl `shouldBe` False

    it "size N" $ do
      let cl = fromList 10 []
      isIn 1 cl `shouldBe` False
      isIn 99 cl `shouldBe` False
      
  describe "get" $ do
    it "simple" $ do
      let cl = fromList 1 [1]
      get cl `shouldBe` 1

    it "empty" $ do
      let cl = fromList 0 []
      evaluate (get cl) `shouldThrow` (== ListIsEmptyException)

  describe "forward" $ do
    it "simple" $ do
      let cl = fromList 2 [1,2]
      (get . forward) cl `shouldBe` 2

    it "empty" $ do
      let cl = fromList 0 []
      evaluate (forward cl) `shouldThrow` (== ListIsEmptyException)

    it "singleton" $ do
      let cl = fromList 1 [1]
      (get . forward) cl `shouldBe` 1

  describe "insert" $ do
    it "simple" $ do
      let cl = fromList 5 [3,2,1]
      get cl `shouldBe` 3
      (toList . insert 5) cl `shouldBe` [3,5,2,1]

    it "empty" $ do
      let cl = (insert 5 . insert 1 . fromList 5) []
      get cl `shouldBe` 1
      toList cl `shouldBe` [1,5]
      
    it "singleton" $ do
      let cl = (insert 2 . fromList 5) [1]
      get cl `shouldBe` 1
      toList cl `shouldBe` [1,2]
      
    it "last" $ do
      let cl = (forward . forward . fromList 5) [3,2,1]
      get cl `shouldBe` 1
      (toList . insert 5) cl `shouldBe` [1,5,3,2]

    it "already" $ do
      let cl = fromList 5 [1,2,3]
      evaluate (insert 3 cl) `shouldThrow` (== AlreadyExistsException)

  describe "remove" $ do
    it "simple" $ do
      let cl = fromList 5 [1,2,3]
      (toList . remove) cl `shouldBe` [1,3]

    it "empty" $ do
      let cl = fromList 0 []
      evaluate (remove cl) `shouldThrow` (== ListIsEmptyException)

    it "singleton" $ do
      let cl = fromList 1 [1]
      (toList . remove) cl `shouldBe` []
      (isEmpty . remove) cl `shouldBe` True
      
    it "last" $ do
      let cl = (forward . fromList 5) [1,2,3]
      (toList . remove) cl `shouldBe` [2,1]

  describe "move" $ do
    it "simple" $ do
      let cl = fromList 5 [1,2,3]
      (get . move 2) cl `shouldBe` 2

    it "empty" $ do
      let cl = fromList 0 []
      evaluate (move 0 cl) `shouldThrow` (== ListIsEmptyException)

    it "not found" $ do
      let cl = fromList 5 [1,2,3]
      evaluate (move 5 cl) `shouldThrow` (== ItemNotFoundException)
      evaluate (move 99 cl) `shouldThrow` (== ItemNotFoundException)

  describe "push" $ do
    it "simple" $ do
      let cl = (forward . push . fromList 5) [1,2,3]
      get cl `shouldBe` 2
      (get . pop) cl `shouldBe` 1

    it "empty" $ do
      let cl = fromList 0 []
      evaluate (push cl) `shouldThrow` (== ListIsEmptyException)

  describe "pop" $ do
    it "simple" $ do
      let cl = (forward . forward . push . fromList 3) [1,2,3]
      get cl `shouldBe` 3
      (get . pop) cl `shouldBe` 1

    it "empty" $ do
      let cl = (push . fromList 0) []
      evaluate (pop cl) `shouldThrow` (== ListIsEmptyException)

    it "empty stack" $ do
      let cl = fromList 1 [1]
      evaluate (pop cl) `shouldThrow` (== StackIsEmptyException)
