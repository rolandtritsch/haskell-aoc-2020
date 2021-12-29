-- |
-- A CircularList that uses a linked-list based on Data.IntMap.
module CircularList where

import Control.Exception
import qualified Data.IntMap as DM

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

type Item = Int

-- | The list.
data CircularList = CircularList Item (DM.IntMap Item) [Item]
  deriving (Show, Eq)

-- | Make a new CircularList from a list.
fromList :: [Int] -> CircularList
fromList [] = CircularList 0 DM.empty []
fromList initial = CircularList (head initial) next []
  where
    next = DM.fromList $ zip initial (tail initial ++ [head initial])

-- | Turn the circular list into a list (from current).
toList :: CircularList -> [Int]
toList (CircularList current next _)
  | DM.null next = []
  | otherwise = current : go (next DM.! current)
  where
    go item
      | item == current = []
      | otherwise = item : go (next DM.! item)
    
-- | Get the length of the list.
size :: CircularList -> Int
size (CircularList _ next _) = DM.size next

-- | Check, if list is empty.
isEmpty :: CircularList -> Bool
isEmpty (CircularList _ next _)
  | DM.null next = True
  | otherwise = False

-- | Check, if item is in the list.
isIn :: Int -> CircularList -> Bool
isIn item (CircularList _ next _) = DM.member item next 

-- | Get the current element.
get :: CircularList -> Item
get (CircularList current next _)
  | DM.null next = throw ListIsEmptyException
  | otherwise = current

-- | Moving current forward.
forward :: CircularList -> CircularList
forward (CircularList current next stack)
  | DM.null next = throw ListIsEmptyException
  | otherwise = CircularList (next DM.! current) next stack


-- | Insert item into list (after current).
--
-- Note: The new item cannot be already in the list.
insert :: Int -> CircularList -> CircularList
insert item (CircularList current next stack)
  | DM.null next = CircularList item (DM.singleton item item) [] 
  | DM.member item next = throw AlreadyExistsException
  | otherwise = CircularList current next' stack
  where
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
pop cl@(CircularList _ _ (to:stack)) = (pop' . move to) cl
  where
    pop' (CircularList current next _) = CircularList current next stack
