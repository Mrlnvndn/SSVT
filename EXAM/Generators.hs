module EXAM.Generators where

import Test.QuickCheck

import EXAM.Relations

-- Normal numbers and structures

genPositive :: Gen Int
genPositive = arbitrary `suchThat` (\x -> x > 0)

genRandomTuple :: Gen (Int, Int)
genRandomTuple = do
  i <- chooseInt (1, 2000)
  y <- chooseInt (1, 2000)
  return (i, y)


-- Relations

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