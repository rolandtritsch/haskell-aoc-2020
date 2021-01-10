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
import Data.Maybe (fromJust)
import Data.String.Utils (strip)
import qualified Text.Regex as R
import Util (inputRaw)
import Prelude

type Allergene = String

type Ingredient = String

type Foods = M.Map Allergene [[Ingredient]]

input :: String -> Foods
input filename = foldl merge M.empty $ foldl (++) [] $ map processLine contents
  where
    contents = lines $ inputRaw filename
    processLine l = map (\a -> (strip a, [ingredients])) allergens
      where
        linePattern = R.mkRegex "^(.*) \\(contains (.*)\\)$"
        tokens = fromJust $ R.matchRegex linePattern l
        (ingredients, allergens) = (splitOn " " (tokens !! 0), splitOn "," (tokens !! 1))
    merge m (a, is) = M.insertWith (++) a is m

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x : xs) ys
  | elem x ys = x : intersect xs ys
  | otherwise = intersect xs ys

intersect' :: Eq a => [[a]] -> [a]
intersect' [] = []
intersect' lists = foldl intersect (head lists) lists

diffr :: Eq a => [a] -> [a] -> [a]
diffr this fromThat = filter (\e -> notElem e this) fromThat

diffMapValues :: Eq v => [v] -> M.Map k [[v]] -> M.Map k [[v]]
diffMapValues this m = M.mapWithKey (\_ vs -> diffValues this vs) m
  where
    diffValues this' vs = map (\fromThat -> diffr this' fromThat) vs

isSingleton :: forall a. [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

isSingleton' :: forall a. [[a]] -> Bool
isSingleton' [[_]] = True
isSingleton' _ = False

flatten :: forall a. [[a]] -> [a]
flatten a = foldl (++) [] a

removeIngredientsByIntersection :: Foods -> Foods
removeIngredientsByIntersection foods = diffMapValues singletons foods
  where
    intersections = flatten $ M.elems $ M.mapWithKey (\_ v -> [intersect' v]) foods
    singletons = flatten $ filter isSingleton intersections

removeIngredientsBySingleton :: Foods -> Foods
removeIngredientsBySingleton foods = diffMapValues singletons foods
  where
    singletons = flatten $ flatten $ filter isSingleton' $ M.elems foods

part1 :: Foods -> Int
part1 foods = length safeToEat
  where
    safeToEat = filter (\i -> elem i incredientsFreeOfAllergens) allIncredients
    allIncredients = flatten $ nub $ flatten $ M.elems foods
    incredientsFreeOfAllergens = nub $ flatten $ flatten $ M.elems foodsFreeOfAllergens
    foodsFreeOfAllergens = go foods False
      where
        go foods' True = foods'
        go foods' False = go foods'' (foods' == foods'')
          where
            foods'' = removeIngredientsBySingleton $ removeIngredientsByIntersection foods'

part2 :: Foods -> Int
part2 foods = M.size foods
