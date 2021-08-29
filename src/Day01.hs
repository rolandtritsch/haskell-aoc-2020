-- |
-- Problem: <https://adventofcode.com/2020/day/1>
--
-- Solution:
--
-- General - List combination/search problem. Build all combinations
-- of list elements. And look for the one that sums up to be 2020.
--
-- Given that '+' is commutative, we only need to build [(1,2), (1,3),
-- (2,3)] out of [1, 2, 3]. And we can stop looking as soon as we find
-- the first tuple that satisfies the condition (making the evaluation
-- of the list (of tuples) lazy (build only the part of the list you
-- need to find the first matching tuple) is another optimisation to
-- consider).
--
-- Another nice variation on the challenge would be to think about
-- how to implement combinationsN. But that is for another day ...
--
-- Part 1 - Do it with pairs.
--
-- Part 2 - Do it with triplets.
module Day01 where

import Data.List (find)
import Data.Maybe (fromJust)
import Util (inputRaw)

type Expense = Int

-- | Read the input file.
input :: String -> [Expense]
input = map read . lines . inputRaw

-- | Returns combinations of pairs.
combinations2 :: [a] -> [(a, a)]
combinations2 [] = []
combinations2 (e : es) = [(e, e') | e' <- es] ++ combinations2 es

-- | Returns combinations of triplets.
combinations3 :: [a] -> [(a, a, a)]
combinations3 [] = []
combinations3 (e : es) = [(e, e', e'') | (e', e'') <- combinations2 es] ++ combinations3 es

-- | Solve part1.
part1 :: [Expense] -> Int
part1 expenses = x * y
  where
    (x, y) =
      fromJust $
        find (\(x', y') -> x' + y' == 2020) $
          combinations2 expenses

-- | Solve part2.
part2 :: [Expense] -> Int
part2 expenses = x * y * z
  where
    (x, y, z) =
      fromJust $
        find (\(x', y', z') -> x' + y' + z' == 2020) $
          combinations3 expenses
