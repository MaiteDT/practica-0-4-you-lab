module LC.Ciphers where

import Data.Char (chr, ord, toUpper)

-- | You can cipher and decipher text with a key
class Cipher a where
  encrypt :: a -> String -> String
  decrypt :: a -> String -> String

-- | Transform ascii letters to upper case, and eliminates the rest
--
-- Examples:
--
-- >>> normalize "ABA"
-- "ABA"
--
-- >>> normalize "ajR$ &(Kl)"
-- "AJRKL"
normalize :: String -> String
normalize = undefined

-- | Shifts an upper case ascii letter by the given step
--
-- Examples:
--
-- >>> 'A' << 0
-- 'A'
--
-- >>> 'D' << 4
-- 'H'
--
-- >>> 'Z' << 2
-- 'B'
(<<) :: Char -> Int -> Char
c << n = undefined

-- | Caeser cipher key
newtype CaesarKey = CaesarKey {offset :: Int} deriving Show

instance Cipher CaesarKey where
  -- |
  -- Examples:
  --
  -- >> encrypt (CaesarKey 7) "PORMIRAZAHABLARAELESPIRITU"
  -- "WVYTPYHGHOHISHYHLSLZWPYPAB"
  encrypt = undefined

  -- |
  -- Examples:
  --
  -- >>> decrypt (CaesarKey 7) "WVYTPYHGHOHISHYHLSLZWPYPAB"
  -- "PORMIRAZAHABLARAELESPIRITU"
  decrypt = undefined

-- | Given a cipher text and a word, finds all the keys and clear texts
-- where the given word appears
--
-- Examples:
--
-- >>> candidates "UNIVERSIDADNACIONAL" "NACIONAL"
-- "[(CaesarKey 0, "UNIVERSIDADNACIONAL")]"
--
-- >>> candidates "WVYTPYHGHOHISHYHLSLZWPYPAB" "ESPIRITU"
-- "[(CaesarKey 7, "PORMIRAZAHABLARAELESPIRITU")]"
candidates :: String -> String -> [(CaesarKey, String)]
candidates = undefined

