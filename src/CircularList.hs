-- |
-- A simple CircularList.
module CircularList where

import Control.Exception
import Data.List (elemIndex)

data ListIsEmptyException = ListIsEmptyException
  deriving (Show, Eq)

instance Exception ListIsEmptyException

type Current = Int
type Item = Int

-- | The list.
data CircularList = CircularList Current [Item]
  deriving (Show, Eq)

-- | Get the length of the list.
lengthCL :: CircularList -> Int
lengthCL (CircularList _ items) = length items

-- | Check, if list is empty.
isEmpty :: CircularList -> Bool
isEmpty (CircularList _ []) = True
isEmpty (CircularList _ _) = False

-- | Get the current element.
get :: CircularList -> Item
get (CircularList _ []) = throw ListIsEmptyException
get (CircularList current items) = items !! current

-- | Moving current forward.
forward :: CircularList -> CircularList
forward (CircularList _ []) = CircularList 0 [] 
forward (CircularList current items)
  | current == length items - 1 = CircularList 0 items
  | otherwise = CircularList (current + 1) items

-- | Moving current backward.
backward :: CircularList -> CircularList
backward (CircularList _ []) = CircularList 0 []
backward (CircularList current items)
  | current == 0 = CircularList (length items - 1) items
  | otherwise = CircularList (current - 1) items

-- | Insert item into list (after current).
insert :: Int -> CircularList -> CircularList
insert item (CircularList current items) = CircularList (current + 1) items'
  where
    (head', tail') = splitAt (current + 1) items
    items' = head' ++ [item] ++ tail'

-- | Remove the current item from the list.
remove :: CircularList -> CircularList
remove (CircularList _ []) = CircularList 0 []
remove (CircularList current items) = CircularList current items'
  where
    (head', tail') = splitAt current items
    items' = head' ++ tail tail'

-- | Move current to ...
move :: Int -> CircularList -> CircularList
move to list@(CircularList _ items) = go (elemIndex to items)
  where
    go Nothing = list
    go (Just current) = CircularList current items
