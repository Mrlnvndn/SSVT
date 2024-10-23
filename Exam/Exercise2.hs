module Exam.Exercise2 where

import Data.List
import Test.QuickCheck

-- == Problem 2 ==
type Rel a = [(a, a)]

-- Calculate the domain of a relation
domain :: (Eq a) => Rel a -> [a]
domain r = map fst r `union` map snd r

-- compose two relations
composition :: (Ord a) => Rel a -> Rel a -> Rel a
composition r s = [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

-- Determine all relations on domain A (powerset of AxA)
allRels :: (Ord a) => [a] -> Rel a
allRels domain = sort $ nub $ [(x, y) | x <- domain, y <- domain]

-- triangular relation on domain A
triangular :: (Eq a) => [a] -> Rel a
triangular domain = [(x, x) | x <- domain]

-- reflexive closure
refClos :: (Eq a) => Rel a -> [a] -> Rel a
refClos r domain = r `union` triangular domain

isReflexive :: (Eq a) => Rel a -> Bool
isReflexive r = all (\x -> (x, x) `elem` r) (domain r)

isIrreflexive :: (Eq a) => Rel a -> Bool
isIrreflexive r = not $ any (uncurry (==)) r

isCoreflexive :: (Eq a) => Rel a -> Bool
isCoreflexive = all (uncurry (==))

isSymmetric :: (Eq a) => Rel a -> Bool
isSymmetric r = all (\(x, y) -> (y, x) `elem` r) r

isAntisymmetric :: (Eq a) => [(a, a)] -> Bool
isAntisymmetric r = all (\(x, y) -> not (hasSymmetricPair x y) || x == y) r
  where
    hasSymmetricPair x y = (y, x) `elem` r

isAsymmetric :: (Eq a) => Rel a -> Bool
isAsymmetric r = isIrreflexive r && isAntisymmetric r

inverse :: (Eq a) => Rel a -> Rel a
inverse r = [(y, x) | (x, y) <- r]

symClos :: (Ord a) => Rel a -> Rel a
symClos r = sort $ nub $ r ++ inverse r

isTransitive :: (Eq a) => [(a, a)] -> Bool
isTransitive r =
  all
    (\(x, z) -> not (existsY x z) || ((x, z) `elem` r))
    [(x, z) | (x, _) <- r, (_, z) <- r]
  where
    existsY x z = any (\y -> (x, y) `elem` r && (y, z) `elem` r) [y | (_, y) <- r]

-- Copied from lab2
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Copied from Lab3.Exercise5
infixr 5 @@

(@@) :: (Eq a) => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

-- Add missing transitive relations recursiveley, stopping if no new relations have been added
trClos :: (Eq a) => Rel a -> Rel a
trClos r
  | r == r' = r
  | otherwise = trClos r'
  where
    r' = r `union` (r @@ r)

-- Check if a relation is serial (from the 2018 exam hints)
isSerial :: (Eq a) => [a] -> [(a, a)] -> Bool
isSerial s r = all (\x -> any (\y -> (x, y) `elem` r) s) s

-- Check if xs is a subset of ys
subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

-- Generate all subsets based on a list of elements
subsets :: [a] -> [[a]]
subsets = foldr (\x acc -> acc ++ map (x :) acc) [[]]

isEquivalence :: (Ord a) => Rel a -> [a] -> Bool
isEquivalence r domain = isSymmetric r && isTransitive r && isReflexive r

-- Create minimal equivalence by applying trClos, symClos and
minimalEquivalence :: (Ord a) => Rel a -> [a] -> Rel a
minimalEquivalence r domain = fixpoint (trClos . symClos . (`refClos` domain)) r
  where
    fixpoint f x
      | x == f x = x
      | otherwise = fixpoint f (f x)

compareRelProperties :: (Ord a) => [a] -> (Rel a -> Bool) -> (Rel a -> Bool) -> String
compareRelProperties domain prop1 prop2 =
  let relations = subsets (allRels domain)
      prop1Rels = filter prop1 relations
      prop2Rels = filter prop2 relations
      prop1Stronger = all (\r -> not (prop1 r) || prop2 r) prop1Rels
      prop2Stronger = all (\r -> not (prop2 r) || prop1 r) prop2Rels
   in case (prop1Stronger, prop2Stronger) of
        (True, True) -> "equivalent"
        (True, False) -> "prop1 is stronger"
        (False, True) -> "prop2 is stronger"
        _ -> "incomparable"

-- Suppose relations are represented as lists of pairs: type Rel a = [(a,a)].
-- 1. b
-- Checks if a relation is coreflexive transitive using the functions provided in the Helpers module
isCoreflexiveTransitive :: (Ord a) => Rel a -> Bool
isCoreflexiveTransitive r = isCoreflexive r && isTransitive r

-- Checks if a relation is asymmetric transitive using the functions provided in the Helpers module
isAsymmetricTransitive :: (Ord a) => Rel a -> Bool
isAsymmetricTransitive r = isAsymmetric r && isTransitive r

-- These properties, which check the properties of the relations, enable us to ue quickcheck
-- Make sure to put in the right properties
prop_incomparable :: [Int] -> Bool
prop_incomparable domain = compareRelProperties domain isCoreflexiveTransitive isAsymmetricTransitive == "incomparable"

prop_equivalent :: [Int] -> Bool
prop_equivalent domain = compareRelProperties domain isCoreflexiveTransitive isAsymmetricTransitive == "equivalent"

prop_rel1Stronger :: [Int] -> Bool
prop_rel1Stronger domain = compareRelProperties domain isCoreflexiveTransitive isAsymmetricTransitive == "prop1 is stronger"

prop_rel2Stronger :: [Int] -> Bool
prop_rel2Stronger domain = compareRelProperties domain isCoreflexiveTransitive isAsymmetricTransitive == "prop2 is stronger"

-- Fill this with the relation of ex2
rel :: Rel Int
rel = [(0, 2), (3, 1)]

-- Fill this with the domain of ex2
dom :: [Int]
dom = [0, 1, 2, 3]

-- Calculate equivalence relation of ex2
minimalEquivalenceRel :: Rel Int
minimalEquivalenceRel = minimalEquivalence rel dom

generateDomain :: Gen [Int]
generateDomain = listOf arbitrary

main :: IO ()
main = do
  putStr "Testing prop_incomparable: "
  quickCheck $ forAll (listOf1 (arbitrary :: Gen Int) `suchThat` (\l -> length l > 2 && length l < 10)) prop_incomparable
  putStr "Testing prop_equivalent: "
  quickCheck $ forAll (listOf1 (arbitrary :: Gen Int) `suchThat` (\l -> length l > 2 && length l < 10)) prop_equivalent
  putStr "Testing prop_rel1Stronger: "
  quickCheck $ forAll (listOf1 (arbitrary :: Gen Int) `suchThat` (\l -> length l > 2 && length l < 10)) prop_rel1Stronger
  putStr "Testing prop_rel2Stronger: "
  quickCheck $ forAll (listOf1 (arbitrary :: Gen Int) `suchThat` (\l -> length l > 2 && length l < 10)) prop_rel2Stronger