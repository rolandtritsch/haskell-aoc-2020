{-|
Problem: <https://adventofcode.com/2020/day/19>

Solution:

General - Looks like a tree walking problem ... and it is not.

To solve this nicely you need to know/understand named capturing
groups in regex. And you need to know that you can ignore the
definitions of the named groups with (?(DEFINE)...).

The regex to solve the test case looks like this ...

^
(?(DEFINE)
  (?P<r4>a)
  (?P<r5>b)
  (?P<r3>((?P>r4)(?P>r5))|((?P>r5)(?P>r4)))
  (?P<r2>((?P>r4)(?P>r4))|((?P>r5)(?P>r5)))
  (?P<r1>((?P>r2)(?P>r3))|((?P>r3)(?P>r2)))
  (?P<r0>((?P>r4)(?P>r1)(?P>r5)))
  (?P<r0>((?P>r4)(?P>r1)(?P>r5)))
)
(?P>r0)
$

It (recursively) defines named groups and is then checking
if r0 is matching the given string.

Part 1 - Go through all messages, match them against the regex and
count the number of matches.

Part 2 - ???
-}
module Day19 where

import Prelude

import Data.List (init, (!!))
import Data.String (lines, endsWith, split, trim, join)
import Data.Binary (toStr)
import Data.Regex as R
import Data.Result (fromOk)

type SateliteImage = {
    regex :: String,
    messages :: [String]
}

input :: String -> SateliteImage
input filename = {regex = regex, messages = messages} where
    messages = init $ lines $ toStr $ unsafePerformIO $ readFile (filename ++ "-messages")
    rules = init $ lines $ toStr $ unsafePerformIO $ readFile (filename ++ "-rules")
    regex = "^(?(DEFINE)" ++ regexRules ++ ")(?P>r0)$" where
        regexRules = join (map processRules rules) "" where
            rid l = head $ split l ":"
            processRules l
                | endsWith l "\"a\"" = "(?P<r" ++ (rid l) ++ ">a)"
                | endsWith l "\"b\"" = "(?P<r" ++ (rid l) ++ ">b)"
                | otherwise = "(?P<r" ++ (rid l) ++ ">" ++ rs ++ ")" where
                    rs = join (map p ids) "|"
                    ids = map (\is -> split (trim is) " ") $ split ((split l ":") !! 1) "|"
                    p is = "(" ++ (join (map (\i -> "(?P>r" ++ i ++ ")") is) "") ++ ")"

matchOk :: R.RunResult -> Boolean
matchOk (R.Match _) = true
matchOk _ = false

part1 :: SateliteImage -> Integer
part1 image = length $ filter matchOk $ map match image.messages where
      pattern = fromOk $ R.compile image.regex []
      match m = R.run m pattern []

part2 :: SateliteImage -> Integer
part2 image = length image.messages
