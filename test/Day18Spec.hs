module Day18Spec where

import Day18
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "testcases" $ do
      let expected = Add (Mul (Add (Mul (Add (Val 1) (Val 2)) (Val 3)) (Val 4)) (Val 5)) (Val 6)
      head (input "./input/Day18p1test.txt") `shouldBe` expected

  -- describe "input'" $ do
  --   it "testcases" $ do
  --     let expected = (Mul (Mul (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))) (Add (Val 5) (Val 6)))
  --     head (input' "./input/Day18p1test.txt") `shouldBe` expected

  describe "toPostfix" $ do
    it "testcases" $ do
      let expression = "1 + 2 * 3 + 4 * 5 + 6"
      (toPostfix $ reverse' expression) `shouldBe` "654321+*+*+"

  describe "toPostfix2" $ do
    it "testcases" $ do
      let expression = "1 + 2 * 3 + 4 * 5 + 6"
      (toPostfix2 $ reverse' expression) `shouldBe` "65+43+21+**"

  describe "eval" $ do
    it "testcases - part1" $ do
      let expression = Add (Mul (Add (Mul (Add (Val 1) (Val 2)) (Val 3)) (Val 4)) (Val 5)) (Val 6)
      eval expression `shouldBe` 71

    it "testcases - part2" $ do
      let expression = (Mul (Mul (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))) (Add (Val 5) (Val 6)))
      eval expression `shouldBe` 231

  describe "part1" $ do
    it "testcases" $ do
      let expected = sum [71,51,26,437,12240,13632]
      part1 (input "./input/Day18p1test.txt") `shouldBe` expected

    it "puzzle" $ do
      part1 (input "./input/Day18p1.txt") `shouldBe` 209335026987

  describe "part2" $ do
    it "testcases" $ do
      let expected = sum [231,51,46,1445,669060,23340]
      part2 (input2 "./input/Day18p1test.txt") `shouldBe` expected

    it "puzzle" $ do
      part2 (input2 "./input/Day18p1.txt") `shouldBe` 33331817392479
