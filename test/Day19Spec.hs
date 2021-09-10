module Day19Spec where

import Day19
import Test.Hspec

run :: IO ()
run = hspec $ do
  describe "input" $ do
    it "input" $ do
      let (SateliteImage _ messages) = input "./input/Day19p1test.txt"
      messages `shouldBe` ["ababbb","bababa","abbbab","aaabbb","aaaabbb"]

  describe "part1" $ do
    it "testcases" $ do
      part1 (input "./input/Day19p1test.txt") `shouldBe` 2

    it "puzzle" $ do
      part1 (input "./input/Day19p1.txt") `shouldBe` 124

  describe "solve" $ do
    it "testcases - part1" $ do
      let expected = ["bbabbbbaabaabba","ababaaaaaabaaab","ababaaaaabbbaba"]
      let image = input "./input/Day19p2test.txt"
      solve image `shouldBe` expected 

    it "testcases - part2 (single)" $ do
      let expected = [
            "babbbbaabbbbbabbbbbbaabaaabaaa"
            ]
      let (SateliteImage rules _) = input "./input/Day19p2test2.txt"
      solve (SateliteImage rules expected) `shouldBe` expected

    it "testcases - part2" $ do
      let expected = [
            "bbabbbbaabaabba",
            "babbbbaabbbbbabbbbbbaabaaabaaa",
            "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
            "bbbbbbbaaaabbbbaaabbabaaa",
            "bbbababbbbaaaaaaaabbababaaababaabab",
            "ababaaaaaabaaab",
            "ababaaaaabbbaba",
            "baabbaaaabbaaaababbaababb",
            "abbbbabbbbaaaababbbbbbaaaababb",
            "aaaaabbaabaaaaababaa",
            "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
            "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
            ]
      let image = input "./input/Day19p2test2.txt"
      solve image `shouldBe` expected 

  describe "part2" $ do
    it "testcases" $ do
      part2 (input "./input/Day19p2test.txt") `shouldBe` 3
      part2 (input "./input/Day19p2test2.txt") `shouldBe` 12

    it "puzzle" $ do
      part2 (input "./input/Day19p2.txt") `shouldBe` 228
