import Test.QuickCheck
import Data.List
import Data.Char
import System.Random
import GHC.Read (list)


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
convChar :: Char -> Char
convChar c 
    | c >= 'a' && c <= 'z' = chr $ ord 'a' + (ord c - ord 'a' + 13) `mod` 26
    | c>= 'A' && c <= 'Z' = chr $ ord 'A' + (ord c - ord 'A' + 13) `mod` 26
    |otherwise = c


rot13 :: [Char] -> [Char]
rot13 li = map convChar li

{-
    elements that can be checked are:
    - The corectness of our encoding by encoding it twice and expecting the same input as result
    - The non-ecoding of an element that is not acharacter
    - The length of our resulting string is the same as our input string
    
    -}

properEncoding :: [Char] -> Bool
properEncoding li = (rot13 . rot13) li == li

checkLength :: [Char] -> Bool
checkLength li = length(rot13 li) == length li

checkNonEncoding:: [Char] -> Bool
checkNonEncoding li = [convChar c | c<-li, not(isAlpha c)]




