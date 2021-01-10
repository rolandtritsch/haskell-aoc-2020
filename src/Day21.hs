{-|
Problem: <https://adventofcode.com/2020/day/21>

Solution:

General - Analyse food. Food has incredients. A given allergene is in
one-of the ingredients. 

Part 1 - Build the intersections to find out which incredients do have
allergens. Find more by looking for allergens with only one ingredient
(search by elimination; sudoku strategy). You need to do theses two
steps/phases recursively until no more "singletons" can be found.

Part 2 - ???
-}
module Day21 where

import Prelude

import Data.List (init, (!!), elem, notElem, nub)
import Data.String (lines, split, trim)
import Data.Binary (toStr)
import Data.Regex as R
import Data.Result (fromOk)
import Data.Map as M

type Allergene = String
type Ingredient = String
type Foods = M.Map Allergene [[Ingredient]]

input :: String -> Foods
input filename = foldl merge M.empty $ foldl (++) [] $ map processLine contents where
    contents = init $ lines $ toStr $ unsafePerformIO $ readFile filename
    processLine l = map (\a -> (trim a, [ingredients])) allergens where
        linePattern = fromOk $ R.compile "^(.*) \\(contains (.*)\\)$" []
        tokens = tail $ R.split l linePattern [R.Trim]
        (ingredients, allergens) =  (split (tokens !! 0) " ", split (tokens !! 1) ",")
    merge m (a, is) = M.updateWithInit a (\is' -> is' ++ is) is m

intersect :: forall a. [a] -> [a] -> [a]
intersect [] _ = []
intersect [x|xs] ys
    | elem x ys = cons x $ intersect xs ys
    | otherwise = intersect xs ys

intersect' :: forall a. [[a]] -> [a]
intersect' [] = []
intersect' lists = foldl intersect (head lists) lists

diffr :: forall a. [a] -> [a] -> [a]
diffr this fromThat = filter (\e -> notElem e this) fromThat

diffMapValues :: forall k v. [v] -> M.Map k [[v]] -> M.Map k [[v]]
diffMapValues this m = M.mapWithKey (\(_, vs) -> diffValues this vs) m where
    diffValues this' vs = map (\fromThat -> diffr this' fromThat) vs

isSingleton :: forall a. [a] -> Bool
isSingleton [_] = true
isSingleton _ = false

isSingleton' :: forall a. [[a]] -> Bool
isSingleton' [[_]] = true
isSingleton' _ = false

flatten :: forall a. [[a]] -> [a]
flatten a = foldl (++) [] a

removeIngredientsByIntersection :: Foods -> Foods
removeIngredientsByIntersection foods = diffMapValues singletons foods where
    intersections = flatten $ M.values $ M.mapWithKey (\(_, v) -> [intersect' v]) foods
    singletons = flatten $ filter isSingleton intersections

removeIngredientsBySingleton :: Foods -> Foods
removeIngredientsBySingleton foods = diffMapValues singletons foods where
    singletons = flatten $ flatten $ filter isSingleton' $ M.values foods

part1 :: Foods -> Integer
part1 foods = length safeToEat where
    safeToEat = filter (\i -> elem i incredientsFreeOfAllergens) allIncredients
    allIncredients = flatten $ nub $ flatten $ M.values foods
    incredientsFreeOfAllergens = nub $ flatten $ flatten $ M.values foodsFreeOfAllergens
    foodsFreeOfAllergens = go foods false where
      go foods' true = foods'
      go foods' false = go foods'' (foods' == foods'') where
          foods'' = removeIngredientsBySingleton $ removeIngredientsByIntersection foods' 

part2 :: Foods -> Integer
part2 foods = M.size foods
