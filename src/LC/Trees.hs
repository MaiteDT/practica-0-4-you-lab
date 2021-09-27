module LC.Trees where

-- | A binary tree is either empty, or has two subtrees as children
data Tree a = Empty | Node {root:: a, left::Tree a, right::Tree a} deriving (Show, Eq)

-- | Number of elements
--
-- Examples:
-- >>> size (Node 4 Empty Empty)
-- 1
--
-- >>> size (Node 4 (Node 5 (Node 8 Empty Empty) (Node 7 Empty Empty)) (Node 6 Empty Empty))
-- 5
size :: Tree a -> Int
size = undefined

-- | Depth of the tree
--
-- Examples:
-- >>> depth Empty
-- 0
--
-- >>> depth (Node 3 (Node 7 Empty Empty) (Node 1 (Node 2 Empty Empty) (Node 9 Empty Empty)) )
-- 3
depth :: Tree a -> Int
depth = undefined

-- | Numberof leafs in a tree
--
-- Examples:
-- >>> width (Node 1 (Node 2 Empty Empty) (Node 9 Empty Empty))
-- 2
--
-- >>> width (Node 3 (Node 7 (Node 8 Empty Empty) (Node 9 Empty Empty)) (Node 1 (Node 2 Empty Empty) (Node 10 Empty Empty)) )
-- 4
width :: Tree a -> Int
width = undefined

-- | Way to travers a tree
data TreeOrder = InOrder | PreOrder | PosOrder

-- | Flattens tree in the given order
--
-- Examples:
-- >>> Flattens (Node 5 (Node 2 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 13 (Node 7 Empty Empty) (Node 12 Empty Empty))) PreOrder
-- [5,2,10,9,13,7,12]
--
-- >>> Flattens (Node 5 (Node 2 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 13 (Node 7 Empty Empty) (Node 12 Empty Empty))) InOrder
-- [10,2,9,5,7,13,12]
--
-- >>> Flattens (Node 5 (Node 2 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 13 (Node 7 Empty Empty) (Node 12 Empty Empty))) PosOrder
-- [10,9,2,7,12,13,5]
flatten :: Tree a -> TreeOrder -> [a]
flatten = undefined

-- | Lenght of the path between the root and an element
--
-- Examples:
-- >>> level (Node 5 (Node 2 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 13 (Node 7 Empty Empty) (Node 12 Empty Empty))) 12
-- 2
--
-- >>> level (Node 5 Empty Empty) 5
-- 0
level :: Eq a => Tree a -> a -> Int
level = undefined

-- | Save nodes at each level
--
-- Examples:
-- >>> levels (Node 5 (Node 2 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 13 (Node 7 Empty Empty) (Node 12 Empty Empty))) PreOrder
-- [[5,[2,13],[10,9,7,12]]]
--
-- >>> levels (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 Empty Empty)) InOrder
-- [[1], [2,5],[3,4]]
levels :: Tree a -> [[a]]
levels = undefined

-- | Searches element on tree
--
-- Examples:
-- >>> contains (Node 1 (Node 2 Empty Empty) (Node 9 Empty Empty)) 7
-- False
--
-- >>> contains (Node 3 (Node 7 (Node 8 Empty Empty) (Node 9 Empty Empty)) (Node 1 (Node 2 Empty Empty) (Node 10 Empty Empty))) 10
-- True
contains :: Eq a => Tree a -> a -> Bool
contains = undefined

-- | Adds element to an ordered tree
--
-- Examples:
-- >>> addOrd (Node 5 (Node 3 (Node 2 Empty Empty) (Node 6 Empty Empty)) (Node 8 (Node 7 Empty Empty) (Node 11 Empty Empty))) 4
-- (Node 5 (Node 3 (Node 2 Empty Empty) (Node 6 (Node 4 Empty Empty) Empty)) (Node 8 (Node 7 Empty Empty) (Node 11 Empty Empty)))
addOrd :: (Ord a) => Tree a -> a -> Tree a
addOrd = undefined
