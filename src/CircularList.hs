-- |
-- A simple CircularList.
module CircularList where

import Control.Exception
import Data.List (elemIndex)

data ListIsEmptyException = ListIsEmptyException
  deriving (Show, Eq)

instance Exception ListIsEmptyException

data ItemNotFoundException = ItemNotFoundException
  deriving (Show, Eq)

instance Exception ItemNotFoundException

data StackIsEmptyException = StackIsEmptyException
  deriving (Show, Eq)

instance Exception StackIsEmptyException

type Current = Int
type Item = Int

-- | The list.
data CircularList = CircularList Current [Item] [Current]
  deriving (Show, Eq)

-- | Make a new CircularList from a list.
fromList :: [Int] -> CircularList
fromList initial = CircularList 0 initial []

-- | Turn the circular list into a list (from current).
toList :: CircularList -> [Int]
toList (CircularList current items _) = tail' ++ head'
  where
    (head', tail') = splitAt current items
    
-- | Get the length of the list.
size :: CircularList -> Int
size (CircularList _ cl _) = length cl

-- | Check, if list is empty.
isEmpty :: CircularList -> Bool
isEmpty (CircularList _ [] _) = True
isEmpty (CircularList _ _ _) = False

-- | Check, if item is in the list.
isIn :: Int -> CircularList -> Bool
isIn item (CircularList _ items _) = elem item items

-- | Get the current element.
get :: CircularList -> Item
get (CircularList _ [] _) = throw ListIsEmptyException
get (CircularList current items _) = items !! current

-- | Moving current forward.
forward :: CircularList -> CircularList
forward (CircularList _ [] _) = throw ListIsEmptyException
forward (CircularList current items stack)
  | current == length items - 1 = CircularList 0 items stack
  | otherwise = CircularList (current + 1) items stack

-- | Moving current backward.
backward :: CircularList -> CircularList
backward (CircularList _ [] _) = throw ListIsEmptyException
backward (CircularList current items stack)
  | current == 0 = CircularList (length items - 1) items stack
  | otherwise = CircularList (current - 1) items stack

-- | Insert item into list (after current).
insert :: Int -> CircularList -> CircularList
insert item (CircularList current items stack) = CircularList current items' stack
  where
    (head', tail') = splitAt (current + 1) items
    items' = head' ++ [item] ++ tail'

-- | Remove the item after the current item from the list.
remove :: CircularList -> CircularList
remove (CircularList _ [] _) = throw ListIsEmptyException
remove cl@(CircularList current items stack)
  | size cl - 1 == current = CircularList (current - 1) (tail items) stack  
  | otherwise = CircularList current items' stack
    where
      (head', tail') = splitAt (current + 1) items
      items' = head' ++ tail tail'

-- | Move current to ...
move :: Int -> CircularList -> CircularList
move _ (CircularList _ [] _) = throw ListIsEmptyException
move to (CircularList _ items stack) = go (elemIndex to items)
  where
    go Nothing = throw ItemNotFoundException
    go (Just current) = CircularList current items stack

-- | Push the current item on the stack.
push :: CircularList -> CircularList
push (CircularList _ [] _) = throw ListIsEmptyException
push (CircularList current items stack) = CircularList current items ((items !! current) : stack)
    
-- | Pop the stack and move to that item.
pop :: CircularList -> CircularList
pop (CircularList _ _ []) = throw StackIsEmptyException
pop list@(CircularList _ _ (to:stack)) = (pop' . move to) list
  where
    pop' (CircularList current items _) = CircularList current items stack
