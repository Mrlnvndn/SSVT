import Data.List
import Data.Char
import Test.QuickCheck


{-
To check if three numbers make a triangle, check that any two sides are greater than the third 

To test generate numbers that satisfy each type of triangle, feed those numbers 
and ensure the result is the same 

-}


data Shape = NoTriangle | Equilateral
 | Isosceles | Rectangular | Other deriving (Eq,Show)

noTriangle :: Integer -> Integer -> Integer -> Bool
noTriangle a b c =
   not ((a + b > c) && (a + c > b) && (b + c > a))

equalateral :: Integer -> Integer -> Integer -> Bool 
equalateral a b c =
   a == b && b == c

rectangular :: Integer -> Integer -> Integer -> Bool
rectangular a b c =
  (a^2 + b^2) == c^2 || (a^2 + c^2) == b^2 || (c^2 + b^2) == a^2

isosceles :: Integer -> Integer -> Integer -> Bool
isosceles a b c =
    (a == b && a /= c) || (a == c && a /= b) || (b == c && b /= a)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
    | noTriangle a b c = NoTriangle
    | equalateral a b c = Equilateral
    | rectangular a b c = Rectangular
    | isosceles a b c = Isosceles
    | otherwise = Other

{-
To test my functions, have functions for each triangle type that generate correct triangles for each type.
After these solutions are generated in the form of list of integer tuples, i pass these into the 
function I've created and get a list of all the outcomes, then use elem to check that this list does not contain,
and False solutions. If so it is true.
-}
genNoTriangle :: Integer -> [(Integer, Integer, Integer)]
genNoTriangle n =
    [(k, j, i) | k <- [1..n], j <- [1..n], i <- [1..n], not ((k + j > i) && (k + i > j) && (j + i > k))]

verifyNoTriangle :: [(Integer, Integer, Integer)] -> Bool
verifyNoTriangle listTri = 
    not False `elem` (map (\ (a, b, c) -> noTriangle a b c) listTri)

-- genNoEquelateral :: Integer -> [(Integer, Integer, Integer)]
-- genNoEquelateral n =
--     [(k, j, i) | k <- [1..n], j <- [1..n], i <- [1..n], not ((k + j > i) && (k + i > j) && (j + i > k))]

-- verifyNoTriangle :: [(Integer, Integer, Integer)] -> Bool
-- verifyNoTriangle listTri = 
--     not False `elem` (map (\ (a, b, c) -> noTriangle a b c) listTri)
    
main :: IO()
main = do
    let result = verifyNoTriangle (genNoTriangle 10)
    print result 

