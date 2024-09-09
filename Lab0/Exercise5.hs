import Data.Char
import Test.QuickCheck

{-
Time spent: 105 minutes
ROT13 is a cipcher used to encode text by shifting each letter forward by 13 
positions. This makes the letter a -> n, b -> o, etc. If moving forward by 13 
positions goes past 'z', it will wrap around at the start of the alphabet and keep going.
This algorithm uses the enumerated value of characters to find the value that is forward 
by 13. 

ROT13 does not change the value of numbers of special characters
-}
rot13 :: [Char] -> [Char]
rot13 c =
    [if ord k >= 97 && ord k <= 122
        then if ord k >= 110
            then chr (ord k - 13)
            else chr (ord k + 13)
    else if ord k >= 65 && ord k <= 90
        then if ord k >= 78
            then chr (ord k - 13)
            else chr (ord k + 13)
    else k
    | k <- c]

{-
QuickCheck testable properties
- rot13 should twice return the original input 
- Capital char should return the a capital char 13 positions forward
- Lowercase char should return the lowercase char 13 positions forward
- If the enum value of the char goes past the enum value of the 'Z' or 'z'
    it should wrap around to the front of the alphabet 
- Numbers should stay the same
- Special characters should stay the same 
-}

-- Twice return input 
prop_twice :: [Char] -> Bool
prop_twice c =
    rot13 (rot13 c) ==  c

-- Capital letters 
genCapitalChar :: [Char]
genCapitalChar = ['A'..'Z']

prop_capitalLetters :: [Char] -> Bool
prop_capitalLetters c =
    [if ord k >= 65 && ord k <= 90
        then if ord k >= 78
            then chr (ord k - 13)
            else chr (ord k + 13)
    else k
    | k <- c] == rot13 c

-- Lowercase letters 
genLowerChar :: [Char]
genLowerChar = ['a'..'z']

prop_lowerLetters :: [Char] -> Bool
prop_lowerLetters c =
    [if ord k >= 97 && ord k <= 122
        then if ord k >= 110
            then chr (ord k - 13)
            else chr (ord k + 13)
    else k
    | k <- c] == rot13 c

-- Numbers and Special char
genNumSpecialChar :: [Char]
genNumSpecialChar = ['!'..'@'] ++ ['['..'`'] ++ ['{'..'~']

prop_numSpecial :: [Char] -> Bool
prop_numSpecial c =
    c == rot13 c


{-
There is not a seperate function for testing if the letters wrap correctly,
since to test this it would be the chars ['A'..'Z'] ++ ['a'..'z'] which is
already tested through the capital and lower case tests.
-}
main :: IO ()
main = do
    quickCheck prop_twice
    quickCheck $ prop_capitalLetters genCapitalChar
    quickCheck $ prop_lowerLetters genLowerChar
    quickCheck $ prop_numSpecial genNumSpecialChar


