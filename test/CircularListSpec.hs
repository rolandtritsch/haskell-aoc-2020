module CircularListSpec where

import CircularList
import Test.Hspec
import Control.Exception

run :: IO ()
run = hspec $ do
  describe "fromList" $ do
    it "simple" $ do
      let cl = fromList [3,2,1]
      toList cl `shouldBe` [3,2,1]

    it "empty" $ do
      let cl = fromList []
      toList cl `shouldBe` []
      isEmpty cl `shouldBe` True

    it "single" $ do
      let cl = fromList [1]
      toList cl `shouldBe` [1]
      isEmpty cl `shouldBe` False
      isIn 1 cl `shouldBe` True

  describe "fromList'" $ do
    it "simple" $ do
      let cl = fromList' 10 [3,2,1]
      toList cl `shouldBe` [3,2,1,4,5,6,7,8,9,10]

    it "empty" $ do
      let cl = fromList' 0 []
      toList cl `shouldBe` []
      isEmpty cl `shouldBe` True

    it "no initial" $ do
      let cl = fromList' 10 []
      toList cl `shouldBe` [1 .. 10]
      isEmpty cl `shouldBe` False
      isIn 1 cl `shouldBe` True
      isIn 10 cl `shouldBe` True
      isIn 11 cl `shouldBe` False
      
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

    it "single" $ do
      let cl = fromList [1]
      isEmpty cl `shouldBe` False
      -- (isEmpty . remove) cl `shouldBe` True
      -- (isEmpty . insert 1 . remove) cl `shouldBe` False
      
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

  describe "insert" $ do
    it "simple" $ do
      let cl = fromList' 5 [3,2,1]
      get cl `shouldBe` 3
      (toList . remove) cl `shouldBe` [3,1,4,5]
      (toList . insert 2 . remove) cl `shouldBe` [3,2,1,4,5]

    it "last" $ do
      let cl = fromList' 5 [3,2,1]
      get cl `shouldBe` 3
      (toList . remove) cl `shouldBe` [3,1,4,5]
      (toList . move 3 . insert 2 . move 5 . remove) cl `shouldBe` [3,1,4,5,2]

  describe "remove" $ do
    it "simple" $ do
      let cl = (remove . fromList) [1,2,3]
      toList cl `shouldBe` [1,3]

    it "empty" $ do
      let cl = fromList []
      evaluate (remove cl) `shouldThrow` (== ListIsEmptyException)

    it "last" $ do
      let cl = (forward . remove . forward . forward . fromList) [1,2,3]
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
