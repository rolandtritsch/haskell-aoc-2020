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
-- Part 1 - So far so good.
--
-- Part 2 - Big surprise: The (initial) list based solution does not work. Way
-- to slow. To get a grip on this I ...
--
-- * ... introduced a CircularList type
-- * ... that was (initially) using a list (means we get no performance
-- improvements, but we get a nice interface)
-- * ... and then I experimented with various data-structures to improve
-- the performance
--
-- At the end the winner was ...
module Day23 where

import CircularList
import Data.Char (digitToInt)
import Data.List (intercalate)
import Util (inputRaw)
import Prelude

type Moves = Int
type Pickup = Int

-- | The State to maintain between moves.
data State = State Moves CircularList [Pickup]
  deriving (Eq, Show) 

-- | Read the input and return the initial state.
input :: String -> State
input filename = State moves' cups' []
  where
    moves' = read first'
    cups' = CircularList 0 (map digitToInt second') []
    (first':second':_) = lines $ inputRaw filename

-- | As part of a move: Remove 3 cups (and put them into pickup).
removeCups :: State -> State
removeCups (State moves cups _) = State moves cups' pickup'
  where
    (cups', pickup') = foldl collectPickup (cups, []) [1..3 :: Int]
    collectPickup (cs, ps) _ = (remove cs, ps ++ [(get . forward) cs])

-- | As part of a move: Select the next destination/current.
selectDestination :: State -> State
selectDestination (State moves cups pickup) = State moves cups' pickup
  where
    possibleDestinations = reverse [1 .. get cups - 1] ++ [maximum (toList cups)]
    (cups', True) = foldl moveToDestination (push cups, False) possibleDestinations
    moveToDestination (cs, True) _ = (cs, True)
    moveToDestination (cs, False) to = go (isIn to cs)
      where
        go True = (move to cs, True)
        go False = (cs, False)

-- | As part of a move: Place the cups from pickup.
placePickupCups :: State -> State
placePickupCups (State moves cups pickup) = State moves (pop cups') []
  where
    cups' = foldl insertPickup cups (reverse pickup)
    insertPickup cs p = insert p cs

-- | As part of a move: Determine the new current cup.
newCurrentCup :: State -> State
newCurrentCup (State moves cups _) = State (moves - 1) (forward cups) []

-- | All the actions of a move.
actions :: State -> State
actions = newCurrentCup . placePickupCups . selectDestination . removeCups

-- | Execute moves (until there are no more moves to make).
executeMoves :: State -> State
executeMoves state@(State 0 _ _) = state
executeMoves state = executeMoves (actions state)

-- | Collect the cups after cup 'label'
collect :: Int -> CircularList -> [Int]
collect label cups = (tail . toList . move label) cups

-- | Turn a list of ints into an int.
ints2Int :: [Int] -> Int
ints2Int ints = read $ intercalate "" $ map show ints

-- | Solve part1.
part1 :: State -> Int
part1 state = ints2Int $ collect 1 cups
  where
    (State _ cups _) = executeMoves state

-- | Take a list of cups and add cups to get the given size.
addCups :: Int -> CircularList -> CircularList
addCups size (CircularList _ cups stack) = CircularList 0 (cups ++ [maximum cups + 1 .. size]) stack

-- | Collect values of the 2 ups after cup 'label'
collect' :: Int -> CircularList -> (Int, Int)
collect' label cups = ((get . forward . move label) cups, (get . forward . forward . move label) cups)

-- | Solve part2.
part2 :: State -> Int
part2 (State _ cups _) = first' * second'
  where
    -- state' = State 100000 (addCups 10000 cups) []
    state' = State 10000000 (addCups 1000000 cups) []
    (State _ cups' _) = executeMoves state'
    (first', second') = collect' 1 cups'
