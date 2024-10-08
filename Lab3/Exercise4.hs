module Exercise4 where

-- import Data.Set

import Control.Applicative
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Property (Prop)

type Rel a = [(a, a)]

-- Time Spent 340 min

{--
Exercise 4:
    1. Write a function for checking whether a relation is serial.
    2. Create two props to test 'isSerial' and test it using quickCheck
    3. Look at the modulo relation: How to test?

## Writing isSerial

The definition: R is serial *if and only if*: ∀x∈S:∃y∈S:(x,y)∈R
We thought about a relation being serial meaning that for every x in the domain
there exists an element of the relation mapping to some other element of the domain.
We assume this means reflexive relations (i.e. [(1,1), (2,2)]) would also suffice.
One part that makes this tricky is that a point can be mapped to multiple other values,
which need to be checked as well.

## Properties to test:
We initially tried to create a generic generator for serial and not-serial Relations using quickcheck, but
as with previous labs we ended up with the issue that we were essentially reimplementing the same twice (which makes
for poor testing).
Therefore we developed one version of the function into the property: everyElMapped,
which tests whether every element of D has a mapping (, which is of course the same, but also the most important property).

Apart from that we used known types of relation that should be serial:
- A simple serial relation, which could be an important edge case as well, is the reflexive closure.
- The cartesian product on the domain, which should be serial.
And we check basic properties of domains and relations:
- If relations are serial we know len(relation) is at least len(domain)
- empty domain is always serial
--}

isSerial :: (Eq a) => [a] -> Rel a -> Bool
isSerial domain relation = all (\e -> any (\(x, _) -> x == e) relation) domain

-- ### Quickcheck generators ###

genTuple :: (Eq a) => [a] -> Gen (a, a)
genTuple domain = do
  index1 <- choose (0, length domain - 1)
  index2 <- choose (0, length domain - 1)
  return (domain !! index1, domain !! index2)

genRelation :: (Eq a) => [a] -> Gen (Rel a)
genRelation [] = return []
genRelation domain = do
  relationSize <- choose (1, 10)
  vectorOf relationSize (genTuple domain)

genDomain :: Gen [Integer]
genDomain = arbitrary

genDomainRelation :: Gen ([Integer], Rel Integer)
genDomainRelation = do
  domain <- genDomain
  relation <- genRelation domain
  return (domain, relation)

genDomainAndModulo :: Gen ([Integer], Integer)
genDomainAndModulo = do
  domain <- genDomain
  modulo <- arbitrary `suchThat` (\x -> x /= 0)
  return (domain, modulo)

-- ### Helper functions for props ###

-- Used a very nice monadic way to get cartesian products from:
--  https://stackoverflow.com/questions/4119730/cartesian-product-of-2-lists-in-haskell
cartesianProduct :: (Eq a, Ord a) => [a] -> [a] -> Rel a
cartesianProduct xs ys = (,) <$> xs <*> ys

getReflexive :: (Ord a) => [a] -> Rel a
getReflexive domain = map (\x -> (x, x)) domain

getMappedRel :: (Eq a) => a -> Rel a -> [a]
getMappedRel x rel = map snd $ filter (\(a, _) -> x == a) rel

-- This function accepts the domain as the first parameter and the `Rel` as the second parameter.
-- Returns true if, for every number/char in domain we have some part of relation mapping it to another
--  number/char in the domain. (Look over domain and check that its mapped counterpart is in domain too)
everyElMapped :: (Eq a) => [a] -> Rel a -> Bool
everyElMapped domain rel = all checkSerial (map (\el -> getMappedRel el rel) domain)
  where
    checkSerial [] = False
    checkSerial mapping = all (\x -> x `elem` domain) mapping

-- ### Properties ###

prop_cartProd :: (Eq a, Ord a) => [a] -> Rel a -> Property
prop_cartProd domain relation = isSerial domain cartProd ==> True
  where
    cartProd = cartesianProduct domain domain

-- if the domain  not empty, the domain should also not be empty and vice versa
prop_nonEmpty :: (Eq a) => [a] -> Rel a -> Property
prop_nonEmpty domain relation = property $ if not (null domain) then not (null relation) else null relation

prop_isMapped :: (Eq a) => [a] -> Rel a -> Property
prop_isMapped domain relation = property $ everyElMapped domain relation == isSerial domain relation

prop_reflexive :: (Ord a) => [a] -> Property
prop_reflexive domain = property $ isSerial domain (getReflexive domain)

{--
Part 3: Discussion around modulo relation
We had some trouble completely understanding what is included in R, because
we initially thought that y had te be a multiple of x like the function:
        modRel :: Integral a => [a] -> a -> [(a, a)]
        modRel domain n = [(y `mod` n, y) | y <- domain]

However calculating the relation for a few n's we found that this did not
result in anything Serial so we looked again at how congruency works.
This resulted in the understanding that y mod n  and x mod n need to result in the
same remainder.

## Is it serial?
The modulo relation is certainly serial. This is most easily proven by first showing that
modRel is reflexive. This results from the fact that for a reflexive relation (x,x)
x % n == x % n regardless of what n is used. Therefore modRel is reflexive.
Earlier we already found out that any reflexive relation is also serial, since
for any x from Domain there is at least one tuple in the relation (x,x).

We can also test this using quickCheck by implementing 'getModuloRelation' and
asserting that for any combination of a generated domain and n the resulting relation
is serial.

This means we do not have to use induction to prove this is the case, but since we already did that here
is the proof using induction as well:

=================== INDUCTION START ======================================
    Problem statement: R = {(x,y) | x `congruent` y % n} is a serial relation.

    Base case: x=y
    When x and y are the same number the pair (x,y) is in R, because regardless of n they will be congruent, as
    we talked about in the explanation above (reflexive).

    Then we take the hypothesis to be true and try to find a reasoning for x+1 having an element y
    such that (x + 1) is equivalent to y mod n.

    If we consider a value y + 1 and insert this into the formula we get: [x+1 is equivalent (y+1) mod n].
    Since we know (from induction hypothesis) that y is congruent to x we can write this as [1 is equivalent to 1 mod n],
    which is of course true.

    Therefor we can state (x, y+1) is in R, which proves that there is a value which x maps to in R, making the relation
    serial.

=================== INDUCTION END ========================================
-}

-- Helper function to keep track of domain in recursion
modRel :: (Integral a) => [a] -> [a] -> a -> Rel a
modRel _ [] _ = []
modRel domain (x : xs) n = [(x, y) | y <- domain, isCongruent x y] ++ (modRel domain xs n)
  where
    isCongruent x y = x `mod` n == y `mod` n

-- Function that gets the relation with all the 'modulo' relations in a domain for
-- a specific n (set modulo)
getModuloRelation :: (Integral a) => [a] -> a -> Rel a
getModuloRelation domain n = modRel domain domain n

prop_modRelationSerial :: (Integral a, Ord a) => [a] -> a -> Property
prop_modRelationSerial domain n = property $ isSerial domain (getModuloRelation domain n)

prop_domainSmallerThanRel :: (Ord a) => [a] -> Rel a -> Property
prop_domainSmallerThanRel domain relation = isSerial domain relation ==> (length domain) <= (length relation)

main = do
  quickCheck $ forAll genDomainRelation (uncurry prop_isMapped)
  quickCheck $ forAll genDomainRelation (uncurry prop_nonEmpty)
  quickCheck $ forAll genDomain prop_reflexive
  quickCheck $ forAll genDomainAndModulo (uncurry prop_modRelationSerial)
