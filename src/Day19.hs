-- |
-- Problem: <https://adventofcode.com/2020/day/19>
--
-- Solution:
--
-- General - Looks like a tree walking problem ... and it is not.
--
-- To solve this nicely you need to know/understand named capturing
-- groups in regex. And you need to know that you can ignore the
-- definitions of the named groups with (?(DEFINE)...).
--
-- The regex to solve the test case looks like this ...
--
-- ^
-- (?(DEFINE)
--   (?P<r4>a)
--   (?P<r5>b)
--   (?P<r3>((?P>r4)(?P>r5))|((?P>r5)(?P>r4)))
--   (?P<r2>((?P>r4)(?P>r4))|((?P>r5)(?P>r5)))
--   (?P<r1>((?P>r2)(?P>r3))|((?P>r3)(?P>r2)))
--   (?P<r0>((?P>r4)(?P>r1)(?P>r5)))
--   (?P<r0>((?P>r4)(?P>r1)(?P>r5)))
-- )
-- (?P>r0)
-- \$
--
-- It (recursively) defines named groups and is then checking
-- if r0 is matching the given string.
--
-- Part 1 - Go through all messages, match them against the regex and
-- count the number of matches.
--
-- Part 2 - ???
module Day19 where

import Data.List (intercalate, isSuffixOf)
import Data.List.Split (splitOn)
import Data.String.Utils (strip)
import Text.Regex.PCRE ((=~))
import Util (inputRaw)
import Prelude

-- | The image.
data SateliteImage = SateliteImage String [String]
  deriving (Eq, Show)

-- | Read the input file and return the image.
input :: String -> SateliteImage
input filename = SateliteImage regex messages
  where
    messages = lines $ inputRaw (filename ++ "-messages")
    rules = lines $ inputRaw (filename ++ "-rules")
    regex = "^(?(DEFINE)" ++ regexRules ++ ")(?P>r0)$"
    regexRules = intercalate "" (map processRules rules)
      where
        rid l = head $ splitOn ":" l
        processRules l
          | isSuffixOf "\"a\"" l = "(?P<r" ++ (rid l) ++ ">a)"
          | isSuffixOf "\"b\"" l = "(?P<r" ++ (rid l) ++ ">b)"
          | otherwise = "(?P<r" ++ (rid l) ++ ">" ++ rs ++ ")"
          where
            rs = intercalate "|" (map p ids)
            ids = map (\is -> splitOn " " (strip is)) $ splitOn "|" ((splitOn ":" l) !! 1)
            p is = "(" ++ (intercalate "" (map (\i -> "(?P>r" ++ i ++ ")") is)) ++ ")"

-- | Return all matching messages.
solve :: SateliteImage -> [String]
solve (SateliteImage regex messages) = map fst $ filter snd matches
  where
    matches = zip messages $ map (\m -> (=~) m regex :: Bool) messages

-- | Solve part1.
part1 :: SateliteImage -> Int
part1 image = length $ solve image

-- | Solve part2.
part2 :: SateliteImage -> Int
part2 (SateliteImage _ messages) = length messages
