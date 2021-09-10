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

import Prelude
import Util (inputRaw)

type Allergene = String

type Ingredient = String

type Foods = M.Map Allergene [[Ingredient]]

-- | Read the input file and return map of foods and ingredients.
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

-- | Intersect two lists. Return common elements.
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x : xs) ys
  | elem x ys = x : intersect xs ys
  | otherwise = intersect xs ys

-- | Build the intersection of a list of lists. Return the common elements.
intersect' :: Eq a => [[a]] -> [a]
intersect' [] = []
intersect' lists = foldl intersect (head lists) lists

-- | Build the right diff between two lists. Return the diff.
diffr :: Eq a => [a] -> [a] -> [a]
diffr this fromThat = filter (\e -> notElem e this) fromThat

-- | Build a (foods) map and diffr the ingredients.
diffMapValues :: Eq v => [v] -> M.Map k [[v]] -> M.Map k [[v]]
diffMapValues this m = M.mapWithKey (\_ vs -> diffValues this vs) m
  where
    diffValues this' vs = map (\fromThat -> diffr this' fromThat) vs

-- | Return true, if list got only one element.
isSingleton :: forall a. [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

-- | Return true, if list of lists got only one list and that list
-- got only one element.
isSingleton' :: forall a. [[a]] -> Bool
isSingleton' [[_]] = True
isSingleton' _ = False

-- | Take a map of foods and remove the ingredience that intersect.
removeIngredientsByIntersection :: Foods -> Foods
removeIngredientsByIntersection foods = diffMapValues singletons foods
  where
    intersections = concat $ M.elems $ M.mapWithKey (\_ v -> [intersect' v]) foods
    singletons = concat $ filter isSingleton intersections

-- | Take a map of foods and remove the ingredience that are singletons.
removeIngredientsBySingleton :: Foods -> Foods
removeIngredientsBySingleton foods = diffMapValues singletons foods
  where
    singletons = concat $ concat $ filter isSingleton' $ M.elems foods

-- | Solve part1.
part1 :: Foods -> Int
part1 foods = length safeToEat
  where
    safeToEat = filter (\i -> elem i incredientsFreeOfAllergens) allIncredients
    allIncredients = concat $ nub $ concat $ M.elems foods
    incredientsFreeOfAllergens = nub $ concat $ concat $ M.elems foodsFreeOfAllergens
    foodsFreeOfAllergens = go foods False
      where
        go foods' True = foods'
        go foods' False = go foods'' (foods' == foods'')
          where
            foods'' = removeIngredientsBySingleton $ removeIngredientsByIntersection foods'

-- | Solve part2.
part2 :: Foods -> Int
part2 foods = M.size foods
