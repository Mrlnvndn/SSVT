module Exercise4 where
import Lab0
import Data.List

-- Time spent: 50 min

{-
Exercise: Create a function which generates a stream of reversible primes below 1000
+ Test it for correctness by
    - 'reversal',
    - 'prime reversibility',
    - 'prime membership',
    - 'reversal symmetry',
    - 'unique values'

Implemented using the descriptions in the assignment, which I thought were quite straightforward.
-}

reversibleStream :: [Integer]
reversibleStream = revs where
    nums :: [Integer] = filter prime [1..10000]
    revs = [x | x <- map reversal nums, prime x]

reversalProp :: Integer -> Bool
reversalProp x = reversal (reversal x) == x

primeReversibilityProp :: Integer -> Bool
primeReversibilityProp x = prime x == prime (reversal x)

primeMembershipProp :: Bool
primeMembershipProp = and [prime x | x <- reversibleStream]

-- Check if the amount of values in the stream is correct (using a value from this amazing quiz
-- https://www.sporcle.com/games/raman22feb1988/emirp-primes)
reversiblePrimeCountProp :: Bool
reversiblePrimeCountProp = length reversibleStream == 260

reversalSymmetryProp:: Bool
reversalSymmetryProp = and [reversal x `elem` revs | x <- revs]
    where revs = reversibleStream

uniqueValuesProp :: Bool
uniqueValuesProp = length reversibleStream == length (nub reversibleStream)

maximumValueProp :: Bool
maximumValueProp = maximum reversibleStream < 10000

main :: IO ()
main = do
    let s = reversibleStream   
    print $ "Testing reversal correctness: " ++ show (all reversalProp s)
    print $ "Testing prime reversibility: " ++ show (all primeReversibilityProp s)
    print $ "Testing prime membership: " ++ show primeMembershipProp
    print $ "Testing prime count: " ++ show reversiblePrimeCountProp
    print $ "Testing reversal symmetry: " ++ show reversalSymmetryProp
    print $ "Testing unique values: " ++ show uniqueValuesProp
    print $ "Testing maximum value: " ++ show maximumValueProp
