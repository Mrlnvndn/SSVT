module Exercise6 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd ( Set(..), unionSet, list2set )
import Exercise3 (symClos)
import Exercise5 (trClos, trClos',comparePairs)

type Rel a = [(a,a)]

-- Time Spent: 60 Minutes

{-  
Exercise 6: Test symClos and trClos

## Test method
  Following next is a list of well chosen properties for the two functions we put under test.
  A random test generation with quickcheck is doable as we just need to call QuickCheck on every chosen property.
  In this specific case we need to specify the type expected in each of the properties.
  Quickcheck will then generate random Rel variables to be tested.

## Properties
We test the following properties for symClos:
- symmetric closures are symmetric (duh...)
- Every elements of the relation are also in the symmetric closure
For trClos:
- Transitive closures are actually transitive
- Every element in R is also in transitive closure
- Idempotence

We test this using some manually crafted relations and their known closures.
Afterwards we test these using QuickCheck:
  meaning we generate random relations and check the properties defined.
-}

-- Property: Symmetry. In a symetrical closure, if we have a pair (a,b) then we should also find (b,a) in the Set
prop_symClosSymmetric :: Ord a => Rel a -> Bool
prop_symClosSymmetric r = all (\(a, b) -> (b, a) `elem` symR) symR
  where symR = symClos r

-- Property: Superset for symClos. Every element contained in the Set has to be contained in the Symmetry closure of that Set
prop_symClosSuperset :: Ord a => Rel a -> Bool
prop_symClosSuperset r = all (`elem` symR) r
  where symR = symClos r

-- Property: Transitivity. Checks for transitivity, if there is (a,b) and (b,c) then there has to be a (a,c)
prop_trClosTransitive :: Ord a => Rel a -> Bool
prop_trClosTransitive r = all (\(a, b, c) -> (a, c) `elem` trR) [(a, b, c) | (a, b) <- trR, (b', c) <- trR, b == b']
  where trR = trClos r

-- Property: Superset for trClos. Every element contained in the Set has to be contained in the transitive closure of that Set
prop_trClosSuperset :: Ord a => Rel a -> Bool
prop_trClosSuperset r = all (`elem` trR) r
  where trR = trClos r

-- Property: Idempotence for trClos. Specifies that applying a transitive clossure to an already transitively closured set, gives the same relation.
prop_trClosIdempotent :: Ord a => Rel a -> Bool
prop_trClosIdempotent r = trR == trClos trR
  where trR = trClos r


-- ### Tests ###

-- Manual Tests for each function
manualTests :: IO ()
manualTests = do
    print $ list2set (symClos [(1,2)]) == list2set ([(1,2), (2,1)])
    print $ list2set (trClos [(1,2), (2,3)]) == list2set [(1,2), (2,3), (1,3)] --Idempotence concept

-- QuickCheck tests with specified types
runQuickCheckTests :: IO ()
runQuickCheckTests = do
    quickCheck (prop_symClosSymmetric :: Rel Int -> Bool)
    quickCheck (prop_symClosSuperset :: Rel Int -> Bool)
    quickCheck (prop_trClosTransitive :: Rel Int -> Bool)
    quickCheck (prop_trClosSuperset :: Rel Int -> Bool)
    quickCheck (prop_trClosIdempotent :: Rel Int -> Bool)

main :: IO ()
main = do
    manualTests
    runQuickCheckTests

