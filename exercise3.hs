import Test.QuickCheck
import Data.List
import Data.Char
import System.Random

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)
{-
    All of the following functions will return true if they correspond
    properties of the following shapes has been determined from Wikipedia and various
    educational content that characterize triangles
-}

checkTriangle :: Integer -> Integer -> Integer -> Bool
checkTriangle a b c = a + b > c && a + c > b && b + c > a

checkEqui :: Integer -> Integer -> Integer -> Bool
checkEqui a b c = a == b && b == c && c == a

checkIso :: Integer -> Integer -> Integer -> Bool
checkIso a b c = (a == b || a == c || b == c) && (a/= c || b/= c || a /= b)

checkRect :: Integer -> Integer -> Integer -> Bool
checkRect a b c
    | a < c && b < c = a^2 + b^2 == c^2
    | b < a && c < a = b^2 + c^2 == a^2
    | a < b && c < b = c^2 + a^2 == b^2
-- This is the main function to call all the sub-possibilities

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | not(checkTriangle a b c) = NoTriangle
    | checkEqui a b c = Equilateral
    | checkIso a b c = Isosceles
    | checkRect a b c = Rectangular
    | otherwise = Other

{-
    In the following section we will verify wether our functions work properly by feeding
    in values that hold true for the function they will be testing

-}
verifTriangle :: Integer -> [Shape]
verifTriangle n = [triangle a b c | a <- [1..n], b <- [1..n], c <- [1..n],
 a + b < c || a+c < b || b+c < a]

verifEqui :: Integer -> [Shape]
verifEqui n = [triangle x x x | x <- [1..n]]

verifIso :: Integer -> [Shape]
verifIso n = [triangle a b c | a <- [1..n], b <- [1..n], c <- [1..n],
 (a == b && a /= c) || (a == c && c /= b) || (b==c && b/= a)]

verifRect :: Integer -> [Shape]
verifRect n = [triangle a b c | a <- [1..n], b <- [1..n], c <- [1..n],
 ( a < c && b < c) || (b < a && c < a) || (a < b && c < b) ]