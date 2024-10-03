module Exercise4 where

-- import Data.Set
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Property (Prop)

type Rel a = [(a,a)]

-- Time Spent .. min

{--
Exercise 4: 
    1. Write a function for checking whether a relation is serial.
    2. Create two props to test 'isSerial' and test it using quickCheck
    3. 

# Writing isSerial

The definition: R is serial *if and only if* ∀x∈S:∃y∈S:(x,y)∈R
We thought about a relation being serial meaning that for every x in the domain
there exists an element of the relation mapping to some other element of the domain
We assume this means reflexive relations (i.e. [(1,1), (2,2)]) would also suffice.

## Properties to test:
We need to be able to generate serial and not-serial Relations using quickcheck
and test if our function correctly identifies them.
--}

getMappedRel :: Eq a => a -> Rel a -> [a]
getMappedRel x rel = map snd $ filter (\(a, _) -> x == a) rel

-- This function accepts the domain as the first parameter and the `Rel` as the second parameter.
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rel = all checkSerial (map (\el -> getMappedRel el rel) domain)  where
    checkSerial [] = False
    checkSerial mapping = all (\x -> x `elem` domain) mapping

-- ### Quickcheck generators ###

genDomain :: Gen [Integer]
genDomain = arbitrary

genDomainAndModulo :: Gen ([Integer], Integer)
genDomainAndModulo = do
    max <- choose(1,15)
    let domain = [0..max]
    modulo <- arbitrary `suchThat` (\x -> x > 0)
    return (domain, modulo)

-- ### Properties ###

getReflexive :: Ord a => [a] -> Rel a
getReflexive domain = map (\x -> (x, x)) domain


prop_reflexive:: Ord a => [a] -> Property
prop_reflexive domain = property $ isSerial domain (getReflexive domain)


-- Helper function to keep track of domain in recursion
modRel :: Integral a => [a] -> [a] -> a -> Rel a
modRel _ [] _ = []
modRel domain (x:xs) n = [(x,y) | y <- domain, isCongruent x y] ++ (modRel domain xs n) where
            isCongruent x y = x `mod` n == y `mod` n

-- Function that gets the relation with all the 'modulo' relations in a domain for
-- a specific n (set modulo)
getModuloRelation :: Integral a => [a] -> a -> Rel a
getModuloRelation domain n = modRel domain domain n


prop_modRelationSerial :: (Integral a, Ord a) => [a] -> a -> Property
prop_modRelationSerial domain n = property $ isSerial domain (getModuloRelation domain n)


main = do
    quickCheck $ forAll genDomain prop_reflexive
    quickCheck $ forAll genDomainAndModulo (uncurry prop_modRelationSerial)

