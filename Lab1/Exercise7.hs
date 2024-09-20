import Data.List
import qualified Data.Set as Set
import Test.QuickCheck
import Test.QuickCheck
import Lecture3

-- Implementation is in book 7.7

-- p = Prop 1
-- q = Prop 2
-- r = Prop 3
--  -- (p -> q) <-> ((-q) -> (-p))
-- form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q)) -- (p -> q) <-> ((-p) -> (-q))
-- form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r) -- ((p -> q) && (q -> r)) -> (p -> r)

sub :: Form -> Set.Set Form
sub (Prop x) = Set.fromList [Prop x]
sub (Neg f) = Set.union (Set.fromList [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = Set.union ( Set.union (Set.fromList [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = Set.union ( Set.union (Set.fromList [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = Set.union ( Set.union (Set.fromList [f]) (sub f1)) (sub f2)


{-
Two properties to test:
1. Self Inclusion
    The set of sub formulaes should contain the original formula 
2. Atomic and Connective count
    The number of atomics + connectives should be the same as the length of the subForm
    However, since sub is using using sets, this won't be true. To test this I will implement
    the subL function which creates the sub formulaes using lists and compare this output to
    the number of atomics and connectives. Then the subForm list is converted to a set and
    checked against the output of the sub function to ensure they are the same. 
-}

{-
- This is the same function as the given sub function, but it uses lists
which allows for repeated items in the output

- This function was created for testing purposes 
-}
subL :: Form -> [Form]
subL (Prop x) = [Prop x]
subL (Neg f) = [Neg f] ++ subL f
subL f@(Cnj [f1,f2]) = ([f] ++ subL f1) ++ subL f2
subL f@(Dsj [f1,f2]) = ([f] ++ subL f1) ++ subL f2
subL f@(Impl f1 f2) = ([f] ++ subL f1) ++ subL f2

-- Connective and atomic count functions from Road to Haskell 7.7
-- Edited for formatting and added Impl condition
ccount :: Form -> Int
ccount (Prop p) = 0
ccount (Neg f) = 1 + (ccount f)
ccount f@(Cnj [f1,f2]) = 1 + (ccount f1) + (ccount f2)
ccount f@(Dsj [f1,f2]) = 1 + (ccount f1) + (ccount f2)
ccount f@(Impl f1 f2) = 1 + (ccount f1) + (ccount f2)

acount :: Form -> Int
acount (Prop p) = 1
acount (Neg f) = acount f
acount f@(Cnj [f1,f2]) = (acount f1) + (acount f2)
acount f@(Dsj [f1,f2]) = (acount f1) + (acount f2)
acount f@(Impl f1 f2) = (acount f1) + (acount f2)

-- Recursive nsub function
nsub :: Form -> Set.Set Form -> Int 
nsub (Prop x) = Set.fromList [Prop x]
nsub (Neg f) = Set.union (Set.fromList [Neg f]) (sub f)
nsub f@(Cnj [f1,f2]) = Set.union ( Set.union (Set.fromList [f]) (sub f1)) (sub f2)
nsub f@(Dsj [f1,f2]) = Set.union ( Set.union (Set.fromList [f]) (sub f1)) (sub f2)
nsub f@(Impl f1 f2) = Set.union ( Set.union (Set.fromList [f]) (sub f1)) (sub f2)

prop_selfInclusion :: Form -> Bool
prop_selfInclusion f =
    f `elem` Set.toList (sub f)

prop_atomicConnectCount :: Form -> Bool
prop_atomicConnectCount f =
    length (subL f) == (ccount f + acount f) && Set.fromList (subL f) == sub f

instance Arbitrary Form where
    arbitrary = sized arbitraryForm

arbitraryForm :: Int -> Gen Form
arbitraryForm 0 = Prop <$> arbitrary  -- base case, just generate atomic propositions
arbitraryForm n = oneof [ Prop <$> arbitrary
                        , Neg <$> arbitraryForm (n-1)
                        , Cnj <$> (sequence [arbitraryForm (n `div` 2), arbitraryForm (n `div` 2)])
                        , Dsj <$> (sequence [arbitraryForm (n `div` 2), arbitraryForm (n `div` 2)])
                        , Impl <$> arbitraryForm (n `div` 2) <*> arbitraryForm (n `div` 2)
                        ]

main :: IO()
main = do
    quickCheck prop_selfInclusion
    quickCheck prop_atomicConnectCount





