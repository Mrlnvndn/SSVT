module Exercise3 where

import Test.QuickCheck

-- Time spent: 60 min

{-
Recognizing triangles
Haskell program which takes a triple of integer values encoding the size of triangle sides

It calculates whether the values are a valid triangle
If it is the function returns the type of traingle:
    either Equilateral, Rectangular, Isosceles or 'Other'

We first test this by using a known triangle for each type and checking if it gets correctly labeled.
After that we generate more test cases automatically and test our Triangle version against each one.

One issue with the current implementation is that we always test it using the first n triples
for each of the triangle types. More issues could perhaps be found if we used something like QuickCheck
to check randomly generated ones (although this would affect the way the triangles are found).

-}


data Shape = NoTriangle | Equilateral
    | Isosceles | Rectangular | Other deriving (Eq,Show)

checkInequality :: Integer -> Integer -> Integer -> Bool
checkInequality a b c = c > a + b || b > a + c || a > b + c

checkEquilateral :: Integer -> Integer -> Integer -> Bool
checkEquilateral a b c = a == b && b == c

checkPythagoras :: Integer -> Integer -> Integer -> Bool
checkPythagoras a b c = a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2

checkEqualLength :: Integer -> Integer -> Integer -> Bool
checkEqualLength a b c = a == b || b == c || a == c

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | checkInequality a b c = NoTriangle
            | checkEquilateral a b c = Equilateral
            | checkPythagoras a b c = Rectangular
            | checkEqualLength a b c = Isosceles
            | otherwise = Other


-- Tests
-- generation strategy inspired by https://learnyouahaskell.com/starting-out#ready-set-go
genPythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
genPythagoreanTriples n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b] , a^2 + b^2 == c^2] 

genEquilateralTriples :: Integer -> [(Integer, Integer, Integer)]
genEquilateralTriples n = [(x,x,x) | x <- [1..n]]

-- For isosceles triples we need to also check whether they are actually triangles and no just equilateral
-- otherwise they result in false positive generation
genIsoscelesTriples :: Integer -> [(Integer, Integer, Integer)]
genIsoscelesTriples n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n],
    a == b || b == c || a == c,
    not $ checkInequality a b c,
    not $ checkEquilateral a b c]

-- utility test function
testTriangle :: (Integer, Integer, Integer) -> Shape -> Bool
testTriangle (a,b,c) shape = triangle a b c == shape

-- Main test function
main :: IO ()
main = do
    print (testTriangle (3,1,1) NoTriangle)
    print (testTriangle (1,1,1) Equilateral)
    print (testTriangle (2,2,1) Isosceles)
    print (testTriangle (3,4,5) Rectangular)
    print (testTriangle (3,4,6) Other)

    let n = 100
    let testRect = [triangle a b c | (a,b,c) <- genPythagoreanTriples n]
    putStrLn $ "All rectangular tests passed: " ++ show (all (==Rectangular) testRect)

    let testEquilateral = [triangle a b c | (a,b,c) <- genEquilateralTriples n]
    putStrLn $ "All equilateral tests passed: " ++ show (all (==Equilateral) testEquilateral)

    let testIsosceles = [triangle a b c | (a,b,c) <- genIsoscelesTriples n]
    putStrLn $ "All isosceles tests passed: " ++ show (all (==Isosceles) testIsosceles)
    
