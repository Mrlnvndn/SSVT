module Exercise6 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd ( Set(..), unionSet )
import Exercise1 (manualMethod, quickMethod, generateRandomValue)
import Exercise3 (symClos)
import Exercise5 (trClos, trClos',comparePairs)

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{-  
    Time Spent: 60 Minutes
    Following next is a list of well chosen properties for the two functions we put under test. A random test generation with quickcheck is doabe
    We just need to call QuickCheck on every chosen property. In this specific case we need to specify the type expected in each of the properties.
    Quickcheck will then generate random Rel variables to be tested.
-}

-- Property: Symmetry. In a symetrical closure, if we have a pair (a,b) then we should also find (b,a) in the Set
prop_symClosSymmetric :: Ord a => Rel a -> Bool
prop_symClosSymmetric r = all (\(a, b) -> (b, a) `elem` symR) symR
  where symR = symClos r

-- Property: Superset for symClos. Every element contained in the Set has to be contained in the Symmetry closure of that Set
prop_symClosSuperset :: Ord a => Rel a -> Bool
prop_symClosSuperset r = all (`elem` symR) r
  where symR = symClos r

-- Property: Transitivity. Checks for transitivity, if ther is (a,b) and (b,c) then there has to be a (a,c)
prop_trClosTransitive :: Ord a => Rel a -> Bool
prop_trClosTransitive r = all (\(a, c) -> any (\b -> (a, b) `elem` trR && (b, c) `elem` trR) [y | (x, y) <- trR, x == a]) trR
  where trR = trClos r

-- Property: Superset for trClos. Every element contained in the Set has to be contained in the transitive closure of that Set
prop_trClosSuperset :: Ord a => Rel a -> Bool
prop_trClosSuperset r = all (`elem` trR) r
  where trR = trClos r

-- Property: Idempotence for trClos. Specifies that applying a transitive clossure to an already transitively closured set, gives the same relation.
prop_trClosIdempotent :: Ord a => Rel a -> Bool
prop_trClosIdempotent r = trR == trClos trR
  where trR = trClos r

-- Manual Tests for each function
exampleTests :: IO ()
exampleTests = do
    print $ symClos [(1,2)] == [(1,2), (2,1)]
    print $ trClos [(1,2), (2,3)] == [(1,2), (2,3), (1,3)] --Idempotence concept



-- QuickCheck tests with specified types
runTests :: IO ()
runTests = do
    quickCheck (prop_symClosSymmetric :: Rel Int -> Bool)
    quickCheck (prop_symClosSuperset :: Rel Int -> Bool)
    quickCheck (prop_trClosTransitive :: Rel Int -> Bool)
    quickCheck (prop_trClosSuperset :: Rel Int -> Bool)
    quickCheck (prop_trClosIdempotent :: Rel Int -> Bool)

main :: IO ()
main = do
    exampleTests
    runTests

