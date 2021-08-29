-- |
-- Problem: <https://adventofcode.com/2020/day/13>
--
-- Solution:
--
-- General - This is a reading problem. Reading it fast once,
-- you start to think about lazy list evaluations. Reading it
-- a second time slow(er), you realize that there is an easy
-- way to calculate the solution, because the minutes you
-- need to wait is the remainder of the departure time by
-- the bus id.
--
-- Part 1 - Find the minimum and calculate the solution.
--
-- Part 2 - We have a system of equations along the lines of ...
--
-- mod x b0 = 0
-- mod x+1 b1 = 0
-- ...
-- mod x+n bn = 0
--
-- We are looking for the smallest x to make this system work.
--
-- The first idea would be to try all numbers until all equations
-- work, but ... this will take a long time. An optimization would
-- be to only try the numbers that we get from the first equation.
--
-- The next optimisation is to pick the equation that is most
-- selective (the max of all bus ids).
--
-- But (not too surprsingly) this works for the testcases, but not
-- to solve the problem.
--
-- Next step is to realize that the system above can be solved
-- using the Chinese Remainder Algorithm. But for that we need
-- to restructure the system to ...
--
-- mod x b0 = b0-0
-- mod x b1 = b1-1
-- ...
-- mod x bn = bn-n
--
-- (Conveniently) The input for the chineseRemainder is the
-- list of [(b0, b0-0), (b1, b1-1), ...] pairs.
--
-- But then you realize that you found a solution for the system,
-- but not the smallest one. You can get the smallest solution
-- by caluculating the mod of the chineseRemainder and the product
-- of all moduli.
--
-- That's it.
module Day13 where

import Data.List (find, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Numeric.Domain.Euclidean (chineseRemainder)
import Util (inputRaw)
import Prelude

-- | The schedule (with all the busses).
data Schedule = Schedule
  { departure :: Int,
    busses :: [Int],
    busses' :: [(Integer, Integer)]
  }

-- | Read the input file.
input :: String -> Schedule
input filename = Schedule {departure = departure', busses = busses'', busses' = busses'''}
  where
    contents = lines $ inputRaw filename
    departure' = read $ head contents
    busses'' = map read $ filter (/= "x") $ splitOn "," (contents !! 1)
    busses''' = map f $ filter (\(b, _) -> b /= "x") $ zip (splitOn "," (contents !! 1)) [0 ..]
      where
        f (b, o) = (b', o')
          where
            b' = read b :: Integer
            o' = b' - o :: Integer

-- | Solve part1.
part1 :: Schedule -> Int
part1 schedule = minutes * bus
  where
    (minutes, bus) = minimum $ map waitTime (busses schedule)
      where
        waitTime b = ((negate $ mod (departure schedule) b) + b, b)

-- | Solve part2 (using an algorithm).
part2' :: Schedule -> Integer
part2' schedule = (fromJust $ find solve possible) - bn
  where
    bs = reverse $ sortOn fst $ map f (busses' schedule)
      where
        f (b, o) = (b, b - o)
    bn = snd $ head bs
    possible = map toInteger [mb, mb * 2 ..]
      where
        mb = fst $ maximum (busses' schedule)
    solve p = all check bs
      where
        check (b, o) = mod (p + o - bn) b == 0

-- | Solve part2 (using a calculation).
part2 :: Schedule -> Integer
part2 schedule = mod cr m
  where
    bs = busses' schedule
    cr = chineseRemainder bs
    m = product $ map fst bs
