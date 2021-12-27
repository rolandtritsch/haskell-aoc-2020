-- |
-- A CircularList that uses a double-linked list based
-- on Data.Map for speed (Data.List is way to slow for
-- large lists).
--
-- Was thinking use something like element -> (next, previous),
-- but decided to go with to maps instead.
module CircularList where

import Control.Exception
import qualified Data.Map as DM

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
data CircularList = CircularList Current (DM.Map Item Item) (DM.Map Item Item) [Current]
  deriving (Show, Eq)

-- | Make a new CircularList from a list.
fromList :: [Int] -> CircularList
fromList [] = CircularList 0 DM.empty DM.empty []
fromList initial = CircularList (head initial) next previous []
  where
    next = DM.fromList $ zip initial (tail initial ++ [head initial])
    previous = DM.fromList $ zip initial ([last initial] ++ init initial)

-- | Turn the circular list into a list (from current).
toList :: CircularList -> [Int]
toList (CircularList current next _ _)
  | DM.null next = []
  | otherwise = current : go (next DM.! current)
  where
    go item
      | item == current = []
      | otherwise = item : go (next DM.! item)
    
-- | Get the length of the list.
size :: CircularList -> Int
size (CircularList _ next _ _) = DM.size next

-- | Check, if list is empty.
isEmpty :: CircularList -> Bool
isEmpty (CircularList _ next _ _)
  | DM.null next = True
  | otherwise = False

-- | Check, if item is in the list.
isIn :: Int -> CircularList -> Bool
isIn item (CircularList _ next _ _) = DM.member item next 

-- | Get the current element.
get :: CircularList -> Item
get (CircularList current next _ _)
  | DM.null next = throw ListIsEmptyException
  | otherwise = current

-- | Moving current forward.
forward :: CircularList -> CircularList
forward (CircularList current next previous stack)
  | DM.null next = throw ListIsEmptyException
  | otherwise = CircularList (next DM.! current) next previous stack

-- | Moving current backward.
backward :: CircularList -> CircularList
backward (CircularList current next previous stack)
  | DM.null next = throw ListIsEmptyException
  | otherwise = CircularList (previous DM.! current) next previous stack

-- | Insert item into list (after current).
--
-- Note: The new item cannot be already in the list.
insert :: Int -> CircularList -> CircularList
insert item (CircularList current next previous stack)
  | DM.null next = CircularList item (DM.singleton item item) (DM.singleton item item) [] 
  | DM.member item next = throw AlreadyExistsException
  | otherwise = CircularList current next' previous' stack
  where
    next' = DM.insert item (next DM.! current) $ DM.insert current item next
    previous' = DM.insert item current $ DM.insert (next DM.! (next DM.! current)) item previous

-- | Remove the item after the current item from the list.
remove :: CircularList -> CircularList
remove (CircularList current next previous stack)
  | DM.null next = throw ListIsEmptyException
  | otherwise = CircularList current next' previous' stack
    where
      next' = DM.insert current (next DM.! (next DM.! current)) $ DM.delete (next DM.! current) next
      previous' = DM.insert (next DM.! (next DM.! current)) current $ DM.delete (next DM.! current) previous

-- | Move current to ...
move :: Int -> CircularList -> CircularList
move item (CircularList _ next previous stack)
  | DM.null next = throw ListIsEmptyException
  | DM.notMember item next = throw ItemNotFoundException
  | otherwise = CircularList item next previous stack

-- | Push the current item on the stack.
push :: CircularList -> CircularList
push (CircularList current next previous stack)
  | DM.null next = throw ListIsEmptyException
  | otherwise = CircularList current next previous (current : stack)
    
-- | Pop the stack and move to that item.
pop :: CircularList -> CircularList
pop (CircularList _ _ _ []) = throw StackIsEmptyException
pop list@(CircularList _ _ _ (to:stack)) = (pop' . move to) list
  where
    pop' (CircularList current next previous _) = CircularList current next previous stack
