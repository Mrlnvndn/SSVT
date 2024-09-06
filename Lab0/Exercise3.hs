import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

-- Excercise 3
-- Time Spent: 40 min

data Shape = NoTriangle | Equilateral
   | Isosceles  | Rectangular | Other deriving (Eq,Show)

canFormTriangle :: Integer -> Integer -> Integer -> Bool
canFormTriangle a b c =
  a+b > c && b+c > a && c+a > b

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
  | not (canFormTriangle a b c) = NoTriangle
  | a == b && b == c = Equilateral
  | a == b || a == c || b == c = Isosceles
  | satisfiesPythagorasTheorem a b c = Rectangular
  | otherwise = Other

satisfiesPythagorasTheorem :: Integer -> Integer -> Integer -> Bool
satisfiesPythagorasTheorem a b c
  | a > b && a > c = a^2 == b^2 + c^2
  | b > a && b > c = b^2 == a^2 + c^2
  | otherwise = c^2 == a^2 + b^2

testTriangle :: (Integer, Integer, Integer) -> Shape -> Bool
testTriangle (a,b,c) shape = triangle a b c == shape

main :: IO ()
main = do
  print (testTriangle (3,1,1) NoTriangle)
  print (testTriangle (1,1,1) Equilateral)
  print (testTriangle (2,2,1) Isosceles)
  print (testTriangle (3,4,5) Rectangular)
  print (testTriangle (3,4,6) Other)