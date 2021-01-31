{-# LANGUAGE RankNTypes #-}

-- |
-- Problem: <https://adventofcode.com/2020/day/21>
--
-- Solution:
--
-- General - Analyse food. Food has incredients. A given allergene is in
-- one-of the ingredients.
--
-- Part 1 - Build the intersections to find out which incredients do have
-- allergens. Find more by looking for allergens with only one ingredient
-- (search by elimination; sudoku strategy). You need to do theses two
-- steps/phases recursively until no more "singletons" can be found.
--
-- Part 2 - ???
module Day21 where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.String.Utils (strip)
import qualified Text.Regex as R
import Util (inputRaw)
import Prelude

type Allergene = String

type Ingredient = String

type Foods = M.Map Allergene [S.Set Ingredient]

input :: String -> Foods
input filename = foldl merge M.empty $ flatten $ map processLine contents
  where
    contents = lines $ inputRaw filename
    processLine l = map (\a -> (strip a, [ingredients])) allergens
      where
        linePattern = R.mkRegex "^(.*) \\(contains (.*)\\)$"
        tokens = fromJust $ R.matchRegex linePattern l
        (ingredients, allergens) = (S.fromList (splitOn " " (tokens !! 0)), splitOn "," (tokens !! 1))
    merge m (a, is) = M.insertWith (++) a is m

intersect :: Ord a => S.Set a -> S.Set a -> S.Set a
intersect s s' = S.intersection s s'

intersect' :: Ord a => [S.Set a] -> S.Set a
intersect' [] = S.empty
intersect' lists = foldl intersect (head lists) lists

diffr :: Ord a => S.Set a -> S.Set a -> S.Set a
diffr this fromThat = S.filter (\e -> S.notMember e this) fromThat

diffMapValues :: Ord v => S.Set v -> M.Map k [S.Set v] -> M.Map k [S.Set v]
diffMapValues this m = M.mapWithKey (\_ vs -> diffValues this vs) m
  where
    diffValues this' vs = map (\fromThat -> diffr this' fromThat) vs

isSingleton :: forall a. S.Set a -> Bool
isSingleton s
  | S.size s == 1 = True
  | otherwise = False

isSingleton' :: forall a. [S.Set a] -> Bool
isSingleton' [s]
  | S.size s == 1 = True
  | otherwise = False
isSingleton' _ = False

flatten :: forall a. [[a]] -> [a]
flatten a = foldl (++) [] a

flatten' :: forall a. [S.Set a] -> [a]
flatten' a = foldl (\l s -> l ++ (S.toList s)) [] a

removeIngredientsByIntersection :: Foods -> Foods
removeIngredientsByIntersection foods = diffMapValues singletons foods
  where
    intersections = flatten $ M.elems $ M.mapWithKey (\_ v -> [intersect' v]) foods
    singletons = S.fromList $ flatten' $ filter isSingleton intersections

removeIngredientsBySingleton :: Foods -> Foods
removeIngredientsBySingleton foods = diffMapValues singletons foods
  where
    singletons = S.fromList $ flatten' $ filter isSingleton $ flatten $ M.elems foods

part1 :: Foods -> Int
part1 foods = length safeToEat
  where
    safeToEat = filter (\i -> elem i incredientsFreeOfAllergens) allIncredients
    allIncredients = flatten' $ nub $ flatten $ M.elems foods
    incredientsFreeOfAllergens = nub $ flatten' $ flatten $ M.elems foodsFreeOfAllergens
    foodsFreeOfAllergens = go foods False
      where
        go foods' True = foods'
        go foods' False = go foods'' (foods' == foods'')
          where
            foods'' = removeIngredientsBySingleton $ removeIngredientsByIntersection foods'

part2 :: Foods -> Int
part2 foods = M.size foods
