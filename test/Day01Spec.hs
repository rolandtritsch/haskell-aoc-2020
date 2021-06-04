module Day01Spec where

import Test.Hspec

import Day01

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      input "./input/Day01p1test.txt" `shouldBe` [1721, 979, 366, 299, 675, 1456] 

  describe "combinations2" $ do
    it "[1,2,3]" $ do
      let expected :: [(Expense, Expense)]
          expected = [(1,2), (1,3), (2,3)]
      combinations2 [1, 2, 3] `shouldBe` expected

    it "testcase" $ do
      let expected :: [(Expense, Expense)]
          expected = [(1721,979),(1721,366),(1721,299),(1721,675),(1721,1456),(979,366),(979,299),(979,675),(979,1456),(366,299),(366,675),(366,1456),(299,675),(299,1456),(675,1456)]
      (combinations2 . input) "./input/Day01p1test.txt" `shouldBe` expected

  describe "combinations3" $ do
    it "[1,2,3,4]" $ do
      let expected :: [(Expense, Expense, Expense)]
          expected = [(1,2,3), (1,2,4), (1,3,4), (2,3,4)]
      combinations3 [1, 2, 3, 4] `shouldBe` expected

    it "testcase" $ do
      let expected :: [(Expense, Expense, Expense)]
          expected = [(1721,979,366),(1721,979,299),(1721,979,675),(1721,979,1456),(1721,366,299),(1721,366,675),(1721,366,1456),(1721,299,675),(1721,299,1456),(1721,675,1456),(979,366,299),(979,366,675),(979,366,1456),(979,299,675),(979,299,1456),(979,675,1456),(366,299,675),(366,299,1456),(366,675,1456),(299,675,1456)]
      (combinations3 . input) "./input/Day01p1test.txt" `shouldBe` expected

  describe "part1" $ do
    it "testcase" $ do
      (part1 . input) "./input/Day01p1test.txt" `shouldBe` 514579

    it "puzzle" $ do
      (part1 . input) "./input/Day01p1.txt" `shouldBe` 858496

  describe "part2" $ do
    it "testcase" $ do
      (part2 . input) "./input/Day01p1test.txt" `shouldBe` 241861950

    it "puzzle" $ do
      (part2 . input) "./input/Day01p1.txt" `shouldBe` 263819430
