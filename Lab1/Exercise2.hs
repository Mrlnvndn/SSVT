module Exercise2 where


-- import Data.List
import System.Random
import Test.QuickCheck
import Debug.Trace

{-
Exercise 2: Implement and test the question from Workshop 1:
    len(Powerset (A)) = 2^n where len(A) = n for a set A

To test it we generate ranges of form [1..n] for n's between 0 and 'maxN'. If we make maxN too large
we find that the result takes way too long to finish. We use quickcheck to pick random numbers, but this
is not really required as we found we could only test up to about 30 before getting a result took too long,
meaning quickCheck will test some ranges multiple times.


Anser question 1:
The cardinality property is hard to test in the sense that for larger values of n,
calculating all subsequences is an intensive tasks, which makes testing large values impractical.

Answer question 2:
If we assume that the formula is a fact we can depend upon, that means we are testing whether
the powerset function actually finds the right amount (length) of subsets. You still have not tested whether
these subsets are correct however.

If we had not proved the cardinality property using induction, we could test it after properly testing the
functionality of the powerset function.


-}


maxN :: Integer
maxN = 20

debug = flip trace

-- Generator for testing integers
genRangeN :: Gen [Integer];
genRangeN = do
    n <- arbitrary `suchThat` (\x -> x > 0 && x Prelude.< maxN)
    -- trace ("Testing for n: " ++ show n)
    return [1..n]


-- Powerset function
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps| ps <- powerset xs] ++ powerset xs

propCardinality :: [a] -> Bool
propCardinality xs = do
    let n = length xs
    let powerLen = length (powerset xs)
    powerLen == 2^n



main = do
    quickCheck $ forAll genRangeN  propCardinality