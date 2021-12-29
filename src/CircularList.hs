-- |
-- A CircularList that uses a linked-list based on Data.Sequence.
--
-- Here are a couple of working/design assumptions ...
--
-- * the inital list is consecutive, means it contains the numbers
-- from 1 to N in a/the given order. And the smallest element is 1.
-- * the linked-list will always have one element it: (0,0). With
-- this element in it the CircularList is considered empty. We need
-- this element because Seq is to the base of 0 and our lists are
-- starting with 1.
-- * to remove an element from the list the predecessor and the
-- successor need to be re-linked. The element it-self will be
-- marked as deleted by making it point to 0.
-- * we will never add/insert an element/item that is bigger than N. 
module CircularList where

import Control.Exception
import Data.List (sort)
import qualified Data.Sequence as DS

data AlreadyExistsException = AlreadyExistsException
  deriving (Show, Eq)

instance Exception AlreadyExistsException

data ItemNotFoundException = ItemNotFoundException
  deriving (Show, Eq)

instance Exception ItemNotFoundException

data ListIsEmptyException = ListIsEmptyException
  deriving (Show, Eq)

instance Exception ListIsEmptyException

data OutOfRangeException = OutOfRangeException
  deriving (Show, Eq)

instance Exception OutOfRangeException

data StackIsEmptyException = StackIsEmptyException
  deriving (Show, Eq)

instance Exception StackIsEmptyException

type Item = Int

-- | The list.
data CircularList = CircularList Item (DS.Seq Item) [Item]
  deriving (Show, Eq)

-- | Make a new CircularList from a list.
fromList :: [Int] -> CircularList
fromList [] = CircularList 0 (DS.singleton 0) []
fromList initial = CircularList (head initial) next []
  where
    next = DS.fromList $ map snd $ sort $ (0,0) : zip initial (tail initial ++ [head initial])

-- | Make a new CircularList of size N (from an initial list).
fromList' :: Int -> [Int] -> CircularList
fromList' 0 _ = CircularList 0 (DS.singleton 0) []
fromList' n [] = CircularList 1 (DS.fromFunction (n + 1) (linkIt n)) []
  where
    linkIt n' i'
      | i' == 0 = 0
      | i' == n' = 1
      | otherwise = i' + 1
fromList' n initial = CircularList (head initial) next []
  where
    initial' = DS.fromList $ map snd $ sort $ (0,0) : zip initial (tail initial ++ [length initial + 1])
    next = initial' DS.>< DS.fromFunction (n - length initial) (linkIt n)
    linkIt n' i'
      | length initial + i' + 1 == n' = head initial
      | otherwise = length initial + i' + 2

-- | Turn the circular list into a list (from current).
toList :: CircularList -> [Int]
toList cl@(CircularList current next _)
  | isEmpty cl = []
  | otherwise = current : go (DS.index next current)
  where
    go item
      | item == current = []
      | otherwise = item : go (DS.index next item)
    
-- | Get the length of the list.
size :: CircularList -> Int
size (CircularList _ next _) = DS.length next - 1

-- | Check, if list is empty.
isEmpty :: CircularList -> Bool
isEmpty (CircularList _ next _) = go (DS.findIndexL ((/=) 0) next)
  where
    go Nothing = True
    go (Just _) = False

-- | Check, if item is in the list.
isIn :: Int -> CircularList -> Bool
isIn item cl@(CircularList _ next _)
  | item < 1 || item > size cl = False
  | otherwise = go (DS.index next item)
  where
    go 0 = False
    go _ = True

-- | Get the current element.
get :: CircularList -> Item
get cl@(CircularList current _ _)
  | isEmpty cl = throw ListIsEmptyException
  | otherwise = current

-- | Moving current forward.
forward :: CircularList -> CircularList
forward cl@(CircularList current next stack)
  | isEmpty cl = throw ListIsEmptyException
  | otherwise = CircularList (DS.index next current) next stack

-- | Insert item into list (after current).
--
-- Note: The new item cannot be already in the list.
-- Note: Item needs to be in the range (we do not append to the list).
insert :: Int -> CircularList -> CircularList
insert item cl@(CircularList current next stack)
  | isIn item cl = throw AlreadyExistsException
  | item < 1 || item > size cl = throw OutOfRangeException
  | otherwise = CircularList current next' stack
  where
    next' = DS.update item (DS.index next current) $ DS.update current item next

-- | Remove the item after the current item from the list.
remove :: CircularList -> CircularList
remove cl@(CircularList current next stack)
  | isEmpty cl = throw ListIsEmptyException
  | otherwise = CircularList current next' stack
    where
      next' = DS.update current (DS.index next (DS.index next current)) $ DS.update (DS.index next current) 0 next

-- | Move current to ...
move :: Int -> CircularList -> CircularList
move item cl@(CircularList _ next stack)
  | isEmpty cl = throw ListIsEmptyException
  | not $ isIn item cl = throw ItemNotFoundException
  | otherwise = CircularList item next stack

-- | Push the current item on the stack.
push :: CircularList -> CircularList
push cl@(CircularList current next stack)
  | isEmpty cl = throw ListIsEmptyException
  | otherwise = CircularList current next (current : stack)
    
-- | Pop the stack and move to that item.
pop :: CircularList -> CircularList
pop (CircularList _ _ []) = throw StackIsEmptyException
pop cl@(CircularList _ _ (to:stack)) = (pop' . move to) cl
  where
    pop' (CircularList current next _) = CircularList current next stack
