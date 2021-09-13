-- |
-- Problem: <https://adventofcode.com/2020/day/4>
--
-- Solution:
--
-- General - Mainly a file parsing problem. Parse/scan all passports. When/while
-- scanning all fields are optional (might be there or not). While checking only
-- cid is optional (all other fields need to be there).
--
-- Question is: What is the right datastructure for this? Could do a Map or a list
-- of key/value pairs or a record with optional fields (Maybe).
--
-- Let's go monadic. Let's use Maybe.
--
-- Part 1 - Scan the file and do the check.
--
-- Part 2 - Scan the file and do the checks (using regexes).
module Day04 where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Text (pack, unpack)
import Text.Regex.Pcre2 (captures, matches)
import Util (inputRaw)
import Prelude

-- | The passport (with all its optional fields).
data Passport = Passport (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe String)
  deriving (Eq, Show)

-- | Make a passport.
makePassport :: [(String, String)] -> Passport
makePassport fields = Passport birthYear issueYear expirationYear height hairColor eyeColor passportID countryID
  where
    birthYear = lookup "byr" fields
    issueYear = lookup "iyr" fields
    expirationYear = lookup "eyr" fields
    height = lookup "hgt" fields
    hairColor = lookup "hcl" fields
    eyeColor = lookup "ecl" fields
    passportID = lookup "pid" fields
    countryID = lookup "cid" fields

-- | The rule(s) that make the passport valid (part1).
isValid :: Passport -> Bool
isValid p =
  isJust birthYear
    && isJust issueYear
    && isJust expirationYear
    && isJust height
    && isJust hairColor
    && isJust eyeColor
    && isJust passportID
  where
    (Passport birthYear issueYear expirationYear height hairColor eyeColor passportID _) = p

-- | The rules that make the passport valid (part2).
isValid2 :: Passport -> Bool
isValid2 p =
  isValidBirthYear birthYear
    && isValidIssueYear issueYear
    && isValidExpirationYear expirationYear
    && isValidHeight height
    && isValidHairColor hairColor
    && isValidEyeColor eyeColor
    && isValidPassportID passportID
  where
    (Passport birthYear issueYear expirationYear height hairColor eyeColor passportID _) = p

-- | The rule that makes the birth year valid.
isValidBirthYear :: Maybe String -> Bool
isValidBirthYear byr = if isJust byr then check (fromJust byr) else False
  where
    p = "^([0-9]*)$"
    check f = if checkFormatted f p then checkValid f p else False
    checkValid f' p' = checkValidRange value
      where
        value = read (result !! 0) :: Int
        result = tail $ map unpack $ captures (pack p') (pack f')
    checkValidRange v = v >= 1920 && v <= 2002

-- | The rule that makes the issue year valid.
isValidIssueYear :: Maybe String -> Bool
isValidIssueYear iyr = if isJust iyr then check (fromJust iyr) else False
  where
    p = "^([0-9]*)$"
    check f = if checkFormatted f p then checkValid f p else False
    checkValid f' p' = checkValidRange value
      where
        value = read (result !! 0) :: Int
        result = tail $ map unpack $ captures (pack p') (pack f')
    checkValidRange v = v >= 2010 && v <= 2020

-- | The rule that makes the experation year valid.
isValidExpirationYear :: Maybe String -> Bool
isValidExpirationYear eyr = if isJust eyr then check (fromJust eyr) else False
  where
    p = "^([0-9]*)$"
    check f = if checkFormatted f p then checkValid f p else False
    checkValid f' p' = checkValidRange value
      where
        value = read (result !! 0) :: Int
        result = tail $ map unpack $ captures (pack p') (pack f')
    checkValidRange v = v >= 2020 && v <= 2030

-- | The rule that makes the height valid.
isValidHeight :: Maybe String -> Bool
isValidHeight hgt = if isJust hgt then check (fromJust hgt) else False
  where
    p = "^([0-9]*)(cm|in)$"
    check f = if checkFormatted f p then checkValid f p else False
    checkValid f' p' = checkValidRange value unit
      where
        (value, unit) = (read (result !! 0) :: Int, result !! 1)
        result = tail $ map unpack $ captures (pack p') (pack f')
    checkValidRange v "cm" = v >= 150 && v <= 193
    checkValidRange v "in" = v >= 59 && v <= 76
    checkValidRange _ _ = False

-- | The rule that makes the hair color valid.
isValidHairColor :: Maybe String -> Bool
isValidHairColor hcl = if isJust hcl then check (fromJust hcl) else False
  where
    p = "^(#[a-f0-9]*)$"
    check f = checkFormatted f p

-- | The rule that makes the eye color valid.
isValidEyeColor :: Maybe String -> Bool
isValidEyeColor ecl = if isJust ecl then check (fromJust ecl) else False
  where
    p = "^(amb|blu|brn|gry|grn|hzl|oth)$"
    check f = checkFormatted f p

-- | The rule that makes the passport id valid.
isValidPassportID :: Maybe String -> Bool
isValidPassportID pid = if isJust pid then check (fromJust pid) else False
  where
    p = "^([0-9]*)$"
    check f = checkFormatted f p

-- | Make sure the field is formatted correctly.
checkFormatted :: String -> String -> Bool
checkFormatted field pattern = matches (pack pattern) (pack field)

-- Read the input file.
input :: String -> [Passport]
input filename = map makePassport passports
  where
    passportLines = map processLine $ splitOn "\n\n" $ inputRaw filename
    passports = map makeFields passportLines
    processLine l = intercalate " " $ splitOn "\n" l
    makeFields l = map keyValue $ splitOn " " l
    keyValue kv = (field !! 0, field !! 1)
      where
        field = splitOn ":" kv

-- | Solve part1.
part1 :: [Passport] -> Int
part1 ps = length $ filter isValid ps

-- | Solve part2.
part2 :: [Passport] -> Int
part2 ps = length $ filter isValid2 ps
