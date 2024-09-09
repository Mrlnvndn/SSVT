import Test.QuickCheck

main :: IO()

{-
We are asked to write QuickCheck tests for the two statements given in Lab0 Exercise 1

To prove that the statements are true for all natural numbers,
you would need to use a proof of induction.

Time spent: 55 minutes 

-}


-- Statement 1
nPowerList :: Integer -> Integer -> Integer
nPowerList n power = sum [k ^ power | k <- [1..n] ]

rightSide1 :: Integer -> Integer
rightSide1 n = (n*(n + 1)*(2*n + 1)) `div` 6

test1 :: Integer -> Bool
test1 n = let a = abs n in nPowerList a 2 == rightSide1 a

-- Statement 2 
rightSide2 :: Integer -> Integer
rightSide2 n = (n * (n + 1) `div` 2)^2

{-
The quickcheck statement is using all positive natural numbers
It is possible that the numbers being tested are repeated 
-}
test2 n  = let a = abs n in nPowerList a 3 == rightSide2 a

main = do 
    putStrLn "== Proof Induction"
    quickCheck test1
    quickCheck test2
