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
-- Part 2 - This takes some more thinking. First we are removing the
-- incredience that we know are free of allergens (from part1).
--
-- Then we need to see that we can remove allergens that only have one
-- ingredient. We do this recursively until no more foods are left over.
module Day21 where

import Data.List (intercalate, nub, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.String.Utils (strip)
import Data.Text (pack, unpack)
import Text.Regex.Pcre2 (captures)
import Util (inputRaw)
import Prelude

type Allergene = String

type Ingredient = String

type Foods = M.Map Allergene [[Ingredient]]

-- | Read the input file and return map of foods and ingredients.
input :: String -> Foods
input filename = foldl merge M.empty $ concat $ map processLine contents
  where
    contents = lines $ inputRaw filename
    processLine l = map (\a -> (strip a, [ingredients])) allergens
      where
        tokens = tail $ map unpack $ captures (pack "^(.*) \\(contains (.*)\\)$") (pack l)
        (ingredients, allergens) = (splitOn " " (tokens !! 0), splitOn ", " (tokens !! 1))
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

-- | Intersect the ingredience of all foods.
intersectIngredience :: Foods -> Foods
intersectIngredience = M.mapWithKey (\_ v -> [intersect' v])

-- | Build the right diff between two lists. Return the diff.
diffr :: Eq a => [a] -> [a] -> [a]
diffr this = filter (flip notElem this)

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
    intersections = concat $ M.elems $ intersectIngredience foods
    singletons = concat $ filter isSingleton intersections

-- | Take a map of foods and remove the ingredience that are singletons.
removeIngredientsBySingleton :: Foods -> Foods
removeIngredientsBySingleton foods = diffMapValues singletons foods
  where
    singletons = concat $ concat $ filter isSingleton' $ M.elems foods

-- | Foods that a free of allergenes.
foodsFreeOfAllergens :: Foods -> Foods
foodsFreeOfAllergens foods = go foods False
  where
    go foods' True = foods'
    go foods' False = go foods'' (foods' == foods'')
      where
        foods'' = removeIngredientsBySingleton $ removeIngredientsByIntersection foods'

-- | All ingredients that are free of allergens.
allIncredientsFreeOfAllergens :: Foods -> [Ingredient]
allIncredientsFreeOfAllergens = nub . concat . nub . concat . M.elems . foodsFreeOfAllergens

-- | Solve part1.
part1 :: Foods -> Int
part1 foods = length safeToEat
  where
    safeToEat = filter (flip elem (allIncredientsFreeOfAllergens foods)) (allIncredients foods)
    allIncredients = concat . nub . concat . M.elems

-- | Go through all foods and find the ingredients that contains allergenes.
ingredientContainsAllergene :: Foods -> [(Ingredient, Allergene)]
ingredientContainsAllergene foods = go foods False []
  where
    go _ True ingredientContainsAllergene' = ingredientContainsAllergene'
    go foods' False ingredientContainsAllergene' = go foods'' (M.null foods'') ingredientContainsAllergene''
      where
        singletons = map finish' $ filter (isSingleton' . snd) $ M.toList $ intersectIngredience foods'
          where
            finish' (a, [[i]]) = (i, a)
            finish' _ = error "part2 go finish: Unexpected pattern match"
        foods'' = diffMapValues (map fst singletons) removed
          where
            removed = foldl delete foods' (map snd singletons)
              where
                delete f a = M.delete a f
        ingredientContainsAllergene'' = ingredientContainsAllergene' ++ singletons

-- | Solve part2.
part2 :: Foods -> String
part2 foods = intercalate "," $ map fst $ sortOn snd $ ingredientContainsAllergene foodsWithAllergence
  where
    foodsWithAllergence = diffMapValues (allIncredientsFreeOfAllergens foods) foods
