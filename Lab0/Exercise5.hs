import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

-- Exercise 1
-- Time Spent: 35 min

rot13 :: [Char] -> [Char]
rot13 = map shiftChar
  where
    shiftChar c
      | isAsciiLower c = chr $ ((ord c - ord 'a' + 13) `mod` 26) + ord 'a'
      | isAsciiUpper c = chr $ ((ord c - ord 'A' + 13) `mod` 26) + ord 'A'
      | otherwise = c

prop_preserveCase :: [Char] -> Bool
prop_preserveCase = all (\c -> isAsciiLower c == isAsciiLower (head (rot13 [c])))

prop_preserveAllChars :: [Char] -> Bool
prop_preserveAllChars chars = length chars == length (rot13 chars)

randomChar :: Gen Char
randomChar = choose (chr 32, chr 126)

randomCharList :: Int -> Gen [Char]
randomCharList n = vectorOf n randomChar

main :: IO ()
main = do
  quickCheck $ forAll (randomCharList 100) prop_preserveCase
  quickCheck $ forAll (randomCharList 100) prop_preserveAllChars
