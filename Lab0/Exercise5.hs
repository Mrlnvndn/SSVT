import Test.QuickCheck
import Data.List
import Data.Char
import System.Random
import GHC.Read (list)

-- Time spent: 45 min
{-
    Specs of ROT13:
    takes 13th letter from used letter and replaces it with it
    Encoding can be undone by reapplying the same Rot process because alphabet = 26 letters 

-}


{-
    Here we take every single char from the list to convert it one by one, we first check if the char is lower of uppercase or none of both -> return same char
    The process is the same for upper and lower case
    in the parenthesis we take the ascii value of our char and compare it to the ascii value of a to get it's relative position to the 
    first letter of the alphabet. We add 13 because that's how the encoding works
    if the modulo is positive, we exceeded the last letter of the alphabet and have to start at the beginning again.
    we then add the result to the ascii character of 'a' or 'A' to get the exact ascii value of our transformed char
    Before returning the value, it is transformed to a char type.

-}

main :: IO ()
main = do
    quickCheck properEncoding
    quickCheck checkLength
    quickCheck $ prop_capitalLetters genCapitalChar
    quickCheck $ prop_lowerLetters genLowerChar
    quickCheck $ prop_numSpecial genNumSpecialChar

convChar :: Char -> Char
convChar c 
    | c >= 'a' && c <= 'z' = chr $ ord 'a' + (ord c - ord 'a' + 13) `mod` 26
    | c>= 'A' && c <= 'Z' = chr $ ord 'A' + (ord c - ord 'A' + 13) `mod` 26
    |otherwise = c


rot13 :: [Char] -> [Char]
rot13 li = map convChar li

{-
    Some elements that can be checked are:
    - The corectness of our encoding by encoding it twice and expecting the same input as result
    - The length of our resulting string is the same as our input string
    - Capital char should return the a capital char 13 positions forward
    - Lowercase char should return the lowercase char 13 positions forward
    - If the enum value of the char goes past the enum value of the 'Z' or 'z'
      it should wrap around to the front of the alphabet 
    - Numbers should stay the same
    - Special characters should stay the same 
    -}

-- Check symmetry
properEncoding :: [Char] -> Bool
properEncoding li = (rot13 . rot13) li == li 
  
-- check that the length stays the same
checkLength :: [Char] -> Bool
checkLength li = length(rot13 li) == length li

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





