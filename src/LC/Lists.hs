-- | Various simple functions over lists
module LC.Lists where

-- | Given a list, remove those that aren't primes and square the rest
--
-- Examples:
-- >>> squarePrimes [1, 2, 3, 4, 5]
-- [4, 9, 25]
squarePrimes :: [Int] -> [Int]
squarePrimes = undefined

-- | Removes any list with an even number and concatenates the rest
--
-- Examples:
-- >>> oddConcat [[1, 2], [3, 5], [], [7]]
-- [3, 5, 7]
oddConcat :: [[Int]] -> [Int]
oddConcat = undefined

-- | If a list is a palindrome
--
-- Examples:
-- >>> isPalindrome [1, 2, 2, 2]
-- True
--
-- >>> isPalindrome [8, 7, 8]
-- True
--
-- >> isPalindrome [1, 2, 3]
-- False
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = undefined

-- | Counts consecutive equal elements.
--
-- Examples:
-- >>> countEquals "aaba"
-- [(2, 'a'), (1, ''b'), (1, 'a')]
countEquals :: Eq a => [a] -> [(Int, a)]
countEquals = undefined

-- | Rotate a list n places.
--
-- Examples:
-- >>> rotate [1, 2, 3] 2
-- [3, 1, 2]
rotate :: [a] -> Int -> [a]
rotate = undefined

-- | Generate all permutations of a list. Order doesn't matter.
--
-- Examples:
-- >>> permutations [1, 2, 3]
-- [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]]
permutations :: [a] -> [[a]]
permutations = undefined

-- | Generate the power set of a list. Order doesn't matter.
--
-- Examples:
-- >>> pow []
-- [[]]
--
-- >>> pow [1, 2]
-- [[], [1], [2], [1, 2]]
pow :: [a] -> [[a]]
pow = undefined
