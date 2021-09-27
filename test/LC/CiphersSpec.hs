module LC.CiphersSpec where

import LC.Ciphers

import Data.Char (isAscii, isAsciiUpper, toUpper, ord, isLetter)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.List (sort)

newtype ValidChar = ValidChar Char deriving Show

instance Arbitrary ValidChar where
  arbitrary = ValidChar <$> elements ['A'..'Z']

newtype ValidString = ValidString {getString :: String} deriving (Eq, Show)

instance Arbitrary ValidString where
  arbitrary = ValidString <$> (listOf . elements $ ['A'..'Z'])

newtype NonASCIIString = NonASCIIString String deriving Show

instance Arbitrary NonASCIIString where
  arbitrary = NonASCIIString . filter (not . isAscii) <$> arbitrary

instance Arbitrary CaesarKey where
  arbitrary = CaesarKey <$> elements [0..25]

instance Ord CaesarKey  where
  CaesarKey k1 <= CaesarKey k2 = k1 <= k2

instance Eq CaesarKey  where
  CaesarKey k1 == CaesarKey k2 = k1 == k2

chDiff :: Char -> Char -> Int
chDiff a b = (ord a - ord b) `mod` 26

spec = do
  describe "normalize" $ do
    prop "only uppercase ascii on result" $
      \x ->
        normalize x `shouldSatisfy` foldl (\xs c -> xs && isAsciiUpper c) True
    prop "ascii character are just upper cased" $
      \(ASCIIString x) -> normalize x `shouldBe` toUpper <$> filter isLetter x
    prop "valid strings are unchanged" $
      \(ValidString x) -> normalize x `shouldBe` x
    prop "deletes non-ascii characters" $
      \(NonASCIIString x) -> normalize x `shouldBe` ""

  describe "shift" $ do
    prop "zero is identity" $
      \(ValidChar x) -> x << 0 `shouldBe` x
    prop "zero mod 26 is identity" $
      \(ValidChar x) n -> x << (26 * n) `shouldBe` x
    prop "applying inverse shift is identity" $
      \(ValidChar x) n -> x << n << negate n `shouldBe` x
    prop "correct shift is applied" $
      \(ValidChar x) n ->
        x << n `shouldSatisfy` \x' -> chDiff x' x == (n `mod` 26)

  describe "caesar cipher" $ do
    prop "zero is identity" $
      \(ValidString x) -> encrypt (CaesarKey 0) x `shouldBe` x
    prop "encrypt and decrypt are inverses" $
      \(ValidString x) k@(CaesarKey _) -> (encrypt k . decrypt k) x `shouldBe` x
    prop "every letter is moved equally" $
      \(ValidString x) k@(CaesarKey n) ->
        zip x (encrypt k x) `shouldSatisfy` all (
        \(cc, ce) -> chDiff ce cc == (n `mod` 26))
  describe "candidates" $ do
    prop "empty string is in all encrypted strings" $
      \x -> sort (candidates x "") `shouldBe` [
        (key, crypt) | k <- [0..25],
        let key = CaesarKey k, let crypt = encrypt key x]
    prop "non empty string with itself has only 0 as its candidate" $
      \(ValidString x) (ValidChar c) -> let xs = c:x in
        candidates xs xs `shouldBe` [(CaesarKey 0, xs)]
    prop "string with superset has no candidates" $
      \(ValidString x1) (ValidChar c) -> candidates x1 (c:x1) `shouldBe` []
