-- |
-- Problem: <https://adventofcode.com/2020/day/23>
--
-- Solution: 
--
-- General - Read careful. There is nothing difficult about this.
-- if you follow the instructions and implement the actions correctly
-- you will find/get the solution.
--
-- One small trick/simplification. The position of current does not matter.
-- And that means we can *just* reorder the list in/with the last action
-- of every move/cycke to make current the first element of the list.
--
-- The alternative was to use a lazy, circular linked-list, but then the
-- cut/paste becomes interesting.
--
-- Part 1 - ???
--
-- Part 2 - ???
module Day23 where

import Data.Char (digitToInt)
import Data.List (find, findIndex, intercalate)
import Data.Maybe (fromJust)
import Util (inputRaw)
import Prelude

data State = State {
    moves :: Int,
    cups :: [Int],
    pickup :: [Int],
    destination :: Int
} deriving (Eq, Show) 

input :: String -> State
input filename = State moves' cups' [] 0
  where
    moves' = read first'
    cups' = map digitToInt second'
    (first':second':_) = lines $ inputRaw filename

removeCups :: State -> State
removeCups state = State (moves state) cups' pickup' (destination state)
  where
    (head', rest') = splitAt 1 (cups state)
    pickup' = take 3 rest'
    cups' = head' ++ (drop 3 rest')

selectDestination :: State -> State
selectDestination state = State (moves state) (cups state) (pickup state) destination'
  where
    possibleDestinations = (reverse [0 .. (head (cups state)) - 1]) ++ [maximum (cups state)]
    destinationLabel' = fromJust $ find (\pd -> elem pd (cups state)) possibleDestinations
    destination' = fromJust $ findIndex (== destinationLabel') (cups state)
    
placePickupCups :: State -> State
placePickupCups state = State (moves state) cups' [] (destination state)
  where
    (head', rest') = splitAt ((destination state) + 1) (cups state)
    cups' = head' ++ (pickup state) ++ rest'
    
newCurrentCup :: State -> State
newCurrentCup state = State moves' cups' [] 0
  where
    moves' = (moves state) - 1
    cups' = tail (cups state) ++ [head (cups state)]

actions :: State -> State
actions = newCurrentCup . placePickupCups . selectDestination . removeCups

executeMoves :: State -> State
executeMoves state
  | (moves state) == 0 = state
  | otherwise = executeMoves (actions state)

collect :: Int -> [Int] -> [Int]
collect label cups' = rest' ++ (init head')
  where
    (head', rest') = splitAt (where' + 1) cups'
    where' = fromJust $ findIndex (== label) cups'

ints2Int :: [Int] -> Int
ints2Int ints = read $ intercalate "" $ map show ints

part1 :: State -> Int
part1 state = ints2Int $ collect 1 $ cups $ executeMoves state

part2 :: State -> Int
part2 state = length $ cups state
