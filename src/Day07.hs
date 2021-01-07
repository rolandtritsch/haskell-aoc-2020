-- |
-- Problem: <https://adventofcode.com/2020/day/7>
--
-- Solution:
--
-- General - Hhhmmm ... to get started, I am going to build two data
-- structures: contains and then (from contains) the reverse of it (isIn).
--
-- Part 1 - Just use isIn (recursively) to find all outer bags for a given bag.
--
-- Part 2 - ???
module Day07 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Util (inputRaw)
import Prelude

type Bag = String

type OuterBag = Bag

type InnerBag = (Integer, Bag)

type Contains = M.Map OuterBag [InnerBag]

type IsIn = M.Map Bag [Bag]

data Bags = Bags Contains IsIn
  deriving (Eq, Show)

input :: String -> Bags
input filename = Bags contains isIn
  where
    contains = M.fromList $ map processLine $ lines $ inputRaw filename
    processLine l = (outer, inners)
      where
        tokens = splitOn " " $ head $ splitOn " contain " l
        outer = (tokens !! 0) ++ " " ++ (tokens !! 1)
        inners = map processInnerBag $ splitOn ", " $ splitOn " contain " l !! 1
        processInnerBag "no other bags." = (0, "other bags")
        processInnerBag ib = (count, bag)
          where
            tokens' = splitOn " " ib
            count = read $ head tokens'
            bag = (tokens' !! 1) ++ " " ++ (tokens' !! 2)
    isIn = foldl processContain M.empty (M.toList contains)
      where
        processContain isIn' (outer, inners) = foldl processInner isIn' inners
          where
            processInner isIn'' (0, _) = isIn''
            processInner isIn'' (_, bag) = M.insertWith (++) bag [outer] isIn''

collectOuters :: IsIn -> Bag -> [Bag]
collectOuters isIn bag = processOuters $ M.lookup bag isIn
  where
    processOuters Nothing = [bag]
    processOuters (Just outers) = (foldl (++) [] $ map (collectOuters isIn) outers) ++ [bag]

part1 :: Bags -> Int
part1 (Bags _ isIn) = length outerBags - 1
  where
    outerBags = nub $ collectOuters isIn "shiny gold"

part2 :: Bags -> Int
part2 (Bags contains _) = M.size contains
