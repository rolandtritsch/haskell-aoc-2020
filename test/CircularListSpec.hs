module CircularListSpec where

import CircularList
import Test.Hspec
import Control.Exception

run :: IO ()
run = hspec $ do
  describe "fromList" $ do
    it "simple" $ do
      let cl = fromList [1,2,3]
      toList cl `shouldBe` [1,2,3]

    it "empty" $ do
      let cl = fromList []
      toList cl `shouldBe` []
      isEmpty cl `shouldBe` True

  describe "toList" $ do
    it "simple" $ do
      let cl = fromList [1,2,3]
      (toList . move 2) cl `shouldBe` [2,3,1]

    it "empty" $ do
      let cl = fromList []
      toList cl `shouldBe` []

  describe "size" $ do
    it "simple" $ do
      let cl = fromList [1]
      size cl `shouldBe` 1

    it "empty" $ do
      let cl = fromList []
      size cl `shouldBe` 0

  describe "isEmpty" $ do
    it "simple" $ do
      let cl = fromList []
      isEmpty cl `shouldBe` True
      (isEmpty . insert 1) cl `shouldBe` False

  describe "isIn" $ do
    it "simple" $ do
      let cl = fromList [1]
      isIn 1 cl `shouldBe` True
      isIn 99 cl `shouldBe` False

    it "empty" $ do
      let cl = fromList []
      isIn 99 cl `shouldBe` False
      
  describe "get" $ do
    it "simple" $ do
      let cl = fromList [1]
      get cl `shouldBe` 1

    it "empty" $ do
      let cl = fromList []
      evaluate (get cl) `shouldThrow` (== ListIsEmptyException)

  describe "forward" $ do
    it "simple" $ do
      let cl = fromList [1,2]
      (get . forward) cl `shouldBe` 2

    it "empty" $ do
      let cl = fromList []
      evaluate (forward cl) `shouldThrow` (== ListIsEmptyException)

    it "singleton" $ do
      let cl = fromList [1]
      (get . forward) cl `shouldBe` 1

  describe "backward" $ do
    it "simple" $ do
      let cl = fromList [1,2]
      (get . backward) cl `shouldBe` 2

    it "empty" $ do
      let cl = fromList []
      evaluate (backward cl) `shouldThrow` (== ListIsEmptyException)

    it "singleton" $ do
      let cl = fromList [1]
      (get . backward) cl `shouldBe` 1

  describe "insert" $ do
    it "simple" $ do
      let cl = (insert 99 . fromList) [1,2]
      get cl `shouldBe` 1
      (get . forward) cl `shouldBe` 99
      (get . forward . forward) cl `shouldBe` 2
      toList cl `shouldBe` [1,99,2]

    it "empty" $ do
      let cl = (insert 99 . fromList) []
      get cl `shouldBe` 99
      (get . forward) cl `shouldBe` 99
      toList cl `shouldBe` [99]

    it "complex" $ do
      let cl = (insert 2 . insert 3 . fromList) [1]
      get cl `shouldBe` 1
      (get . forward) cl `shouldBe` 2
      (get . forward . forward) cl `shouldBe` 3
      (get . forward . forward . forward) cl `shouldBe` 1
      toList cl `shouldBe` [1,2,3]

    it "first" $ do
      let cl = (insert 99 . fromList) [1,2,3]
      toList cl `shouldBe` [1,99,2,3]

    it "last" $ do
      let cl = (forward . forward . insert 99 . backward . fromList) [1,2,3]
      toList cl `shouldBe` [1,2,3,99]

  describe "remove" $ do
    it "simple" $ do
      let cl = (remove . fromList) [1,2,3]
      toList cl `shouldBe` [1,3]

    it "empty" $ do
      let cl = fromList []
      evaluate (remove cl) `shouldThrow` (== ListIsEmptyException)

    it "last" $ do
      let cl = (forward . remove . backward . fromList) [1,2,3]
      toList cl `shouldBe` [2,3]

  describe "move" $ do
    it "simple" $ do
      let cl = (move 2 . fromList) [1,2,3]
      get cl `shouldBe` 2

    it "empty" $ do
      let cl = fromList []
      evaluate (move 0 cl) `shouldThrow` (== ListIsEmptyException)

    it "not found" $ do
      let cl = fromList [1,2,3]
      evaluate (move 99 cl) `shouldThrow` (== ItemNotFoundException)

    it "complex" $ do
      let cl = (move 3 . fromList) [1,2,3]
      get cl `shouldBe` 3
      (get .forward) cl `shouldBe` 1

  describe "push" $ do
    it "simple" $ do
      let cl = (forward . push . fromList) [1,2,3]
      get cl `shouldBe` 2
      (get . pop) cl `shouldBe` 1

    it "empty" $ do
      let cl = fromList []
      evaluate (push cl) `shouldThrow` (== ListIsEmptyException)

  describe "pop" $ do
    it "simple" $ do
      let cl = (forward . forward . push . fromList) [1,2,3]
      get cl `shouldBe` 3
      (get . pop) cl `shouldBe` 1

    it "empty" $ do
      let cl = (push . fromList) []
      evaluate (pop cl) `shouldThrow` (== ListIsEmptyException)

    it "empty stack" $ do
      let cl = fromList [1]
      evaluate (pop cl) `shouldThrow` (== StackIsEmptyException)
