-- |
-- A CircularList that uses a linked list based on Data.Sequence
-- for speed (Data.List is way to slow for large lists).
--
-- Here are a couple of working/design assumptions ...
--
-- * the inital list is consecutive, means it contains the numbers
-- from 1 to N in a/the given order. And the smallest element is 1.
-- * to remove an element from the list the predecessor and the
-- successor need to be re-linked. The element it-self will be marked
-- as deleted by making it point to 0.
module CircularList where

import Control.Exception
import qualified Data.Sequence as DS

data ListIsEmptyException = ListIsEmptyException
  deriving (Show, Eq)

instance Exception ListIsEmptyException

data ItemNotFoundException = ItemNotFoundException
  deriving (Show, Eq)

instance Exception ItemNotFoundException

data AlreadyExistsException = AlreadyExistsException
  deriving (Show, Eq)

instance Exception AlreadyExistsException

data StackIsEmptyException = StackIsEmptyException
  deriving (Show, Eq)

instance Exception StackIsEmptyException

type Current = Int
type Item = Int

-- | The list.
data CircularList = CircularList Current (DS.Seq Item) [Current]
  deriving (Show, Eq)

-- | Make a new CircularList from a list.
fromList :: [Int] -> CircularList
fromList [] = CircularList 0 DS.empty []
fromList initial = CircularList (head initial) next []
  where
    next = DS.fromList $ map snd $ sortBy fst $ (0,0) : zip initial (tail initial ++ [head initial])

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
size (CircularList _ next _) = DS.length next

-- | Check, if list is empty.
isEmpty :: CircularList -> Bool
isEmpty (CircularList _ next _)
  | DS.null next = True
  | otherwise = False

-- | Check, if item is in the list.
isIn :: Int -> CircularList -> Bool
isIn item (CircularList _ next _)
  | item <= 1 || item >= DS.length next - 1 = False
  | otherwise = go (DM.index next item)
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
insert :: Int -> CircularList -> CircularList
insert item cl@(CircularList current next stack)
  | isEmpty cl = CircularList item initial' [] 
  | isIn item cl = throw AlreadyExistsException
  | otherwise = CircularList current next' stack
  where
    initial' = DS.fromList $ replicate item 0 ++ [item]
    next' = DM.insert item (next DM.! current) $ DM.insert current item next

-- | Remove the item after the current item from the list.
remove :: CircularList -> CircularList
remove (CircularList current next stack)
  | DM.null next = throw ListIsEmptyException
  | otherwise = CircularList current next' stack
    where
      next' = DM.insert current (next DM.! (next DM.! current)) $ DM.delete (next DM.! current) next

-- | Move current to ...
move :: Int -> CircularList -> CircularList
move item (CircularList _ next stack)
  | DM.null next = throw ListIsEmptyException
  | DM.notMember item next = throw ItemNotFoundException
  | otherwise = CircularList item next stack

-- | Push the current item on the stack.
push :: CircularList -> CircularList
push (CircularList current next stack)
  | DM.null next = throw ListIsEmptyException
  | otherwise = CircularList current next (current : stack)
    
-- | Pop the stack and move to that item.
pop :: CircularList -> CircularList
pop (CircularList _ _ []) = throw StackIsEmptyException
pop list@(CircularList _ _ (to:stack)) = (pop' . move to) list
  where
    pop' (CircularList current next _) = CircularList current next stack
