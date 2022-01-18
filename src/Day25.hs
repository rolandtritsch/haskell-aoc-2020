-- |
-- Problem: <https://adventofcode.com/2020/day/25>
--
-- Solution:
--
-- General - Interesting one. Struggled to read this right and get all of
-- the numbers/concepts (value, subject, key, ...) right. At least the
-- parsing of the input is easy ...
--
-- Main insight was that you can calculate the next value/subject from the
-- previous one with a loop size of 1. And this will obviously speed up the
-- brute forcing of the loop size.
--
-- Part 1 - Brute force the card loop size and use it to calculate the door
-- encryption key.
--
-- Part 2 - Happy Holidays!!!
module Day25 where

import Util (inputRaw)
import Prelude

type Key = Int
type Keys = (Key, Key)

-- | read the input file.
input :: String -> Keys
input filename = (cardPK, doorPK)
  where
    (cardPK : doorPK : _) = map read $ lines $ inputRaw filename

initialValue :: Int
initialValue = 1

superSecretSubject :: Int
superSecretSubject = 7

superSecretSeed :: Int
superSecretSeed = 20201227

-- | transform a subject number.
transform :: Int -> Int -> Int -> Int
transform 0 value _subject = value 
transform loopSize value subject = transform (loopSize - 1) value' subject
  where
    value' = mod (value * subject) superSecretSeed

-- | brute force the public key to get the loopSize.
brute :: Int -> Int -> Key -> Int
brute loopSize value key
  | transform 1 value superSecretSubject == key = loopSize
  | otherwise = brute (loopSize + 1) (transform 1 value superSecretSubject) key

-- | solve part1.
part1 :: Keys -> Int
part1 (cardPK, doorPK) = transform cardLoopSize initialValue doorPK
  where
    cardLoopSize = brute 1 initialValue cardPK

-- | solve part2.
part2 :: Keys -> Int
part2 (cardPK, _doorPK) = cardPK
