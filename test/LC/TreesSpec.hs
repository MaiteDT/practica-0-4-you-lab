-- | Unit tests for various simple functions over lists
module LC.TreesSpec where

import LC.Trees
import Data.List (intersect, nub)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck


instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbitrarySizedTree


arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree 0 = return Empty
arbitrarySizedTree n = do
  genRoot <- arbitrary
  leftSize <- choose (0, n-1)
  genLeft <- arbitrarySizedTree leftSize
  genRight <- arbitrarySizedTree ((n-1) - leftSize)
  return (Node genRoot genLeft genRight)

crunch :: Tree Int -> [Int]
crunch Empty = []
crunch (Node e l r) = e:(crunch l ++ crunch r)

nonNegative :: (Tree Int -> Int) -> Tree Int -> Expectation
nonNegative f t = f t `shouldSatisfy` (>=0)

ceilDiv :: Int -> Int -> Int
ceilDiv a b = ceiling $ fromIntegral a/fromIntegral b

log2 :: Int -> Int
log2 = floor . logBase 2 . fromIntegral

isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

widthBound :: Int -> Int -> Bool
widthBound s w = w <= ceilDiv s 2

nodesTwoChildren :: (Eq a, Integral a) => Tree a -> Int
nodesTwoChildren Empty = 0
nodesTwoChildren (Node x l r) 
  | l /= Empty && r /= Empty = 1 + nodesTwoChildren l + nodesTwoChildren r
  | otherwise = nodesTwoChildren l + nodesTwoChildren r

completeTree :: Tree a -> Maybe Int
completeTree Empty = Just 0
completeTree (Node x a b) =
    case (completeTree a, completeTree b) of
        (Just da, Just db) | da == db -> Just (da + 1)
        _ -> Nothing

nn :: Tree Int -> Int
nn Empty = 0
nn (Node x Empty Empty) = 0
nn (Node x l r) = 1 + nn l + nn r

flattenLength :: Tree Int -> Expectation
flattenLength t =  
  let x = size t in 
    (length $ flatten t PreOrder) == x &&  (length $ flatten t InOrder)  == x &&  (length $ flatten t PosOrder) == x `shouldBe` True

checkContains:: Int -> Tree Int -> Expectation
checkContains e Empty = contains Empty e `shouldBe` False
checkContains _ t = contains t (head $ crunch t) `shouldBe` True

flattenElem :: Tree Int -> Expectation
flattenElem t = (length $ intersect (intersect (flatten t PreOrder) (flatten t InOrder))(flatten t PosOrder))  `shouldSatisfy` (== size t)

numLeafs:: Tree Int -> Expectation
numLeafs Empty = width Empty `shouldSatisfy` (== nodesTwoChildren Empty)
numLeafs t = (width t - nodesTwoChildren t) `shouldBe` 1

nodesRefEmpty :: Tree Int -> Int
nodesRefEmpty Empty = 1
nodesRefEmpty (Node x Empty Empty) = 2
nodesRefEmpty (Node x l Empty) = 1 + nodesRefEmpty l
nodesRefEmpty (Node x Empty r) = 1 + nodesRefEmpty r
nodesRefEmpty (Node x l r) =  nodesRefEmpty l + nodesRefEmpty r

isPow2 ::  [Int]->Int-> Bool
isPow2 x i = (length x) <= 2^i

boundedSize:: Tree Int -> Expectation
boundedSize Empty = depth Empty `shouldSatisfy` (==0) 
boundedSize t =  depth t `shouldSatisfy` let s = length (crunch t) in
        (\d -> d <= s && log2 s <= d)

referencesEmpty :: Tree Int -> Expectation
referencesEmpty t = size t  `shouldSatisfy` (< nodesRefEmpty t)

maxNodeLevel :: Tree Int -> Expectation
maxNodeLevel t =  (and $ zipWith (\i x -> isPow2 x  i) [0..] (levels t)) `shouldBe` True

maxLevesLeafs:: Tree Int -> Expectation 
maxLevesLeafs t 
  |  t == Empty = size t `shouldNotBe` x
  | completeTree t /= Nothing = (size t) `shouldBe` x 
  | otherwise = size t `shouldNotBe` x
  where x = (2*(width t)-1)

maxNodesDepth:: Tree Int -> Expectation
maxNodesDepth t = size t `shouldSatisfy`(<= 2^(depth t) - 1)

minDepth:: Tree Int -> Expectation
minDepth t = depth t `shouldSatisfy` (>= (log2((length $ crunch t) + 1)))

minLevels:: Tree Int -> Expectation
minLevels t = (length $ levels t) `shouldSatisfy` (>= (log2((length $ crunch t) + 1)))

nonNegativeLevel :: Tree Int -> Int -> Expectation
nonNegativeLevel t x = level t x `shouldSatisfy` (>=0)

propCompleteTree ::(Tree Int -> Int) ->  Tree Int -> Int -> Expectation
propCompleteTree f t x
  |  t == Empty = f t `shouldNotBe` x
  | completeTree t /= Nothing = (f t) `shouldBe` x 
  | otherwise = f t `shouldNotBe` x

spec = do
  describe "size" $ do
    it "empty tree has no elements" $
      size Empty `shouldBe` 0
    prop "non-negative size" $ nonNegative size
    prop "number of elements is same as flatten tree" $
      \t -> size t `shouldBe` length (crunch t)
    prop "tree of size n has n + 1 null references"$ referencesEmpty 
    prop "maximum of (2^h-1) nodes if its height is h" $ maxNodesDepth 
    prop "tree of size n has log2(n+1) of minimun height" $ minDepth 
    prop "a tree has n internal nodes, the size is N = 2n + 1" $
      \t -> propCompleteTree size t (2*(nn t) + 1)
    prop "a tree has a total of N nodes, the number of internal nodes is I = (N – 1)/2" $
      \t -> propCompleteTree size t ((nn t) * 2 + 1)
  describe "depth" $ do
    prop "depth is non negative" $ nonNegative depth
    prop "depth is bounded by size" $ boundedSize
  describe "width" $ do
    prop "width is non negative" $ nonNegative width
    prop "width is bounded by size" $
      \t -> width t `shouldSatisfy` (widthBound . length . crunch $ t)
    prop "width is one more than the number of nodes two children" $  numLeafs 
    prop "a tree has n internal nodes, the width is L = n + 1" $
      \t -> propCompleteTree width t (nn t + 1)   
    prop "level of a node is non-negative" $ 
      \t x -> nonNegativeLevel t x
  describe "contains" $ do 
    prop "contains elem in empty tree" $
      and [contains Empty y | y <- [1..]] `shouldBe` False
    prop "contains elem in tree" $ checkContains
  describe "flatten" $ do
    prop "all flattenings of the same length" $ flattenLength 
    prop "all flattenings of the same elems" $ flattenElem 
  describe "addOrder" $ do
    prop "increase length by 1" $ 
      \t x -> (length $ crunch $ addOrd t x)- (length $ crunch t) `shouldBe` 1
    prop "add elem ok" $ 
      \t x -> Prelude.elem x (crunch $ addOrd t x) `shouldBe` True
  describe "levels" $ do
      prop "maximum of 2^l nodes at level" $ maxNodeLevel 
      prop "tree with L leaves has at least ⌈ Log2L ⌉ + 1 levels" $ \t -> propCompleteTree size t (2* (width t) - 1)
      prop "tree of size n has log2(n+1) of minimun height" $ minLevels 
