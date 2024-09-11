import Data.List
import Test.QuickCheck

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-
1. factorial n = factorial (n-1) * n 
2. factorial n is always positive
3. factorial n should be divisble by n-1, n-2,..0
-}

prop_factorialNMinus1 :: Integer -> Property
prop_factorialNMinus1 n =
    n > 0 && n < 1000 ==> factorial n == factorial (n-1) * n

prop_greaterOrEqualN :: Integer -> Property
prop_greaterOrEqualN n =
    n > 0 && n < 1000 ==> factorial n >= n

modToZero :: Integer -> Integer -> Bool
modToZero factN n
    | n == 1 = True 
    | mod factN n == 0 = modToZero factN (n-1)
    | otherwise = False

prop_division :: Integer -> Property
prop_division n =
    n > 0 && n < 1000 ==> modToZero (factorial n) n

{-
For my tests I am only running the range from 0 - 1000 for stack overflow. This allows everyone in the 
group to run the file without any problems.
-}
main :: IO()
main = do
    quickCheck prop_factorialNMinus1
    quickCheck prop_greaterOrEqualN
    quickCheck prop_division
