-- | Unit tests for various simple functions over lists
module LC.ListsSpec where

import LC.Lists

import Data.List (intersect, nub)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype CharSet = CharSet {getChars :: String} deriving (Eq, Show)

instance Arbitrary CharSet where
  arbitrary = CharSet . nub . getASCIIString <$> arbitrary

newtype ShortList = ShortList {getList :: [Int]} deriving (Eq, Show)

instance Arbitrary ShortList where
  arbitrary = ShortList <$> elements [[1..n] | n <- [1..8]]

mirror :: ASCIIString -> String
mirror (ASCIIString s) = s ++ reverse s

oddPalindrom :: ASCIIString -> Char -> String
oddPalindrom (ASCIIString s) c = s ++ [c] ++ reverse s

flatReplicate :: String -> Int -> String
flatReplicate s n = concat $ [replicate n a | a <- s]

countConst :: CharSet -> Positive Int -> Expectation
countConst (CharSet x) (Positive n) =
  countEquals (flatReplicate x n) `shouldBe` [(n, a) | a <- x]

accRepl :: String -> (Int, Char) -> String
accRepl xs (i, a) = xs ++ replicate i a

countVar :: CharSet -> Expectation
countVar (CharSet x) = let xs = zip [1..length x] x in
        countEquals (foldl accRepl "" xs) `shouldBe` xs

rotateId :: ASCIIString -> Expectation
rotateId (ASCIIString x) = rotate x (length x) `shouldBe` x

rotateEvenly :: ASCIIString -> Positive Int -> Expectation
rotateEvenly (ASCIIString x) (Positive n) =
  let r = rotate x n in
        (take (length x) . drop n ) (cycle x) `shouldBe` r

permId :: ShortList -> Expectation
permId (ShortList x) = permutations x `shouldContain` [x]

permSameLength :: ShortList -> Expectation
permSameLength (ShortList x) =
  permutations x `shouldSatisfy` all (\p -> length p == length x)

permSameElems :: ShortList -> Expectation
permSameElems (ShortList x) =
  permutations x `shouldSatisfy` all (\p -> intersect x p == x)

powLength :: ShortList -> Expectation
powLength (ShortList x) = pow x `shouldSatisfy` (==2^length x) . length

powEmpty :: ShortList -> Expectation
powEmpty (ShortList x) = pow x `shouldContain` [[]]

powFull :: ShortList -> Expectation
powFull (ShortList x) = pow x `shouldContain` [x]

powUnique :: ShortList -> Expectation
powUnique (ShortList x) = let xs = pow x in
  [
    (a, b) | a <- xs, b <- xs, a /= b
  ]
  `shouldSatisfy` all (
    \(a, b) -> let i = intersect a b in i /= a || i /= b
  )

tIsPrime :: Int -> Bool
tIsPrime p = length [n  | n <- [2..p], p `mod` n == 0] == 1

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

spec = do
  describe "squarePrimes" $ do
    prop "non-primes are removed" $
      \x -> squarePrimes [a^2 | a <- x] `shouldBe` []
    prop "all roots are primes" $
      \x -> intSqrt <$> squarePrimes x `shouldSatisfy` all tIsPrime

  describe "oddConcat" $ do
    prop "lists with even numbers are removed" $
      \xs -> oddConcat [2:x | x <- xs] `shouldBe` []
    prop "lists with only odd numbers are preserved" $
      \xs -> let xsf = filter odd <$> xs in
        oddConcat xsf `shouldBe` concat xsf

  describe "isPalindrome" $ do
    prop "mirrored lists are palindromes" $
      \x -> mirror x `shouldSatisfy` isPalindrome
    prop "mirrored with an extra middle char is palindrome" $
      \x c -> oddPalindrom x c `shouldSatisfy` isPalindrome

  describe "countEquals" $ do
    prop "equally repeated elements are counted equally" countConst
    prop "differently repeated elemets are counted differently" countVar

  describe "rotate" $ do
    prop "rotation by lenght is identity" rotateId
    prop "rotate evenly" rotateEvenly

  describe "permutations" $ do
    prop "contains the original set" $ permId
    prop "all the same lenght" permSameLength
    prop "contain the same elements" permSameElems
    it "preserves elements in each permutation" $
      let ps = permutations [1..5] in nub ps `shouldBe` ps

  describe "pow" $ do
    prop "there are 2^n subsets" powLength
    prop "empty list is contained" powEmpty
    prop "full list is contained" powFull
    prop "subsets have at least a different element" powUnique
