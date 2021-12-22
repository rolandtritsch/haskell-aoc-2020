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
-- of every move/cycle to make current the first element of the list.
--
-- The alternative was to use a lazy, circular linked-list, but then the
-- cut/paste becomes interesting.
--
-- Part 1 - ???
--
-- Part 2 - ???
module Day23 where

import Data.Char (digitToInt)
import Data.List (find, elemIndex, intercalate)
import Data.Maybe (fromJust)
import Util (inputRaw)
import Prelude

type Moves = Int
type Cup = Int
type Pickup = Int
type Destination = Int

-- | The State to maintain between moves.
data State = State Moves [Cup] [Pickup] Destination
  deriving (Eq, Show) 

-- | Read the input and return the initial state.
input :: String -> State
input filename = State moves' cups' [] 0
  where
    moves' = read first'
    cups' = map digitToInt second'
    (first':second':_) = lines $ inputRaw filename

-- | As part of a move: Remove 3 cups (and put them into pickup).
removeCups :: State -> State
removeCups (State moves cups _ destination) = State moves cups' pickup' destination
  where
    (head', rest') = splitAt 1 cups
    pickup' = take 3 rest'
    cups' = head' ++ drop 3 rest'

-- | As part of a move: Select the next destination.
selectDestination :: State -> State
selectDestination (State moves cups pickup _) = State moves cups pickup destination'
  where
    possibleDestinations = reverse [0 .. head cups - 1] ++ [maximum cups]
    destinationLabel' = fromJust $ find (flip elem cups) possibleDestinations
    destination' = fromJust $ elemIndex destinationLabel' cups

-- | As part of a move: Place the cups from pickup.
placePickupCups :: State -> State
placePickupCups (State moves cups pickup destination) = State moves cups' [] destination
  where
    (head', rest') = splitAt (destination + 1) cups
    cups' = head' ++ pickup ++ rest'

-- | As part of a move: Determine the new current cup.
newCurrentCup :: State -> State
newCurrentCup (State moves cups _ _) = State (moves - 1) cups' [] 0
  where
    cups' = tail cups ++ [head cups]

-- | All the actions of a move.
actions :: State -> State
actions = newCurrentCup . placePickupCups . selectDestination . removeCups

-- | Execute moves (until there are no more moves to make).
executeMoves :: State -> State
executeMoves state@(State 0 _ _ _) = state
executeMoves state = executeMoves (actions state)

-- | Collect the cups after cup 'label'
collect :: Int -> [Int] -> [Int]
collect label cups = rest' ++ (init head')
  where
    (head', rest') = splitAt (where' + 1) cups
    where' = fromJust $ elemIndex label cups

-- | Turn a list of ints into an int.
ints2Int :: [Int] -> Int
ints2Int ints = read $ intercalate "" $ map show ints

-- | Solve part1.
part1 :: State -> Int
part1 state = ints2Int $ collect 1 cups
  where
    (State _ cups _ _) = executeMoves state

part2 :: State -> Int
part2 (State _ cups _ _) = length cups
