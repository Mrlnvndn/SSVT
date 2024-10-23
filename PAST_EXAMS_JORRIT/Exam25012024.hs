module Exam25012024 where
import EXAM.Relations
import Data.List (permutations)
import Data.List (nub, sort)
import Test.QuickCheck
import Test.QuickCheck.Property
import EXAM.LTS (matchOutputs, IOLTS, nextTransitions')

{-
Practice exam resit 2024:

Start: 14:30

-}

-- Problem 2
-- b. (Its incomparable as said on paper)

coreflexiveTransitive :: Ord a => Rel a -> [a] -> Bool
coreflexiveTransitive a d = isCoReflexive a && isTransitive a

asymmetricTransitive :: Ord a => Rel a -> [a] -> Bool
asymmetricTransitive a d = isAsymmetric a d && isTransitive a

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

allRelationsDomain :: (Ord a) => [a] -> Rel a
allRelationsDomain domain = nub $ [(x,y) | x <- domain, y <- domain]

compareRelProps :: Ord a => [a] -> (Rel a -> [a] -> Bool) -> (Rel a -> [a] -> Bool) -> String
compareRelProps domain prop1 prop2 =
  let relations = subsets $ allRelationsDomain domain
      prop1Relations = filter (\r -> prop1 r domain) relations
      prop2Relations = filter (\r -> prop2 r domain) relations
      prop1Stronger = all (\r -> prop1 r domain && prop2 r domain) prop1Relations
      prop2Stronger = all (\r -> prop2 r domain && prop1 r domain) prop2Relations
  in if prop1Stronger && prop2Stronger then "equivalent"
     else if prop1Stronger then "prop1 is stronger"
     else if prop2Stronger then "prop2 is stronger"
     else "incomparable"


genDomain :: Gen [Integer]
genDomain = listOf (arbitrary :: Gen Integer) `suchThat` (\l -> length l > 2 && length l < 10)

prop_incomparable domain = compareRelProps domain coreflexiveTransitive asymmetricTransitive == "incomparable"

problem2 = quickCheck $ forAll genDomain prop_incomparable

-- == Problem 3 ==

{-
Creating properties for numNRI:
- Find the explicit formula and compare to it

-}

-- Had to change it to Integer to no overflow when testing with quickCheck
genPositive :: Gen Integer
genPositive = arbitrary `suchThat` (\x -> x > 0)

numNRIr :: Integer -> Integer
numNRIr 0 = 0
numNRIr 1 = 0
numNRIr n = 2 ^ (n ^ 2 - 1) + 2 ^ (2 * n - 2) * numNRIr (n - 1)



-- Working out is in
numNRIrExplicit :: Integer -> Integer
numNRIrExplicit n = 2^(n^2) - 2*2^(n^2 - n)

prop_equalToExplicit :: Integer -> Bool
prop_equalToExplicit n = numNRIr n == numNRIrExplicit n

prop_increasing :: Integer -> Bool
prop_increasing n = numNRIr (n+1) > numNRIr n

problem3 = do
    quickCheck $ forAll genPositive prop_equalToExplicit
    quickCheck $ forAll genPositive prop_increasing





-- == Problem 4 ==
type State = Integer

type Label = String

type LabeledTransition = (State, Label, State)

type LTS = ([State], [Label], [LabeledTransition], State)

tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper

delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper


juiceImpl :: LTS
juiceImpl = ([1 .. 3], ["?apple", "?orange", "!applejuice"], [(1, "?apple", 1), (1, "?orange", 2), (2, "!applejuice", 3)], 1)

juiceModel :: LTS
juiceModel = ([1 .. 5], ["?apple", "?orange", "!applejuice", "!orangejuice"], 
              [(1, "?apple", 2), (1, "?orange", 3), (2, "!applejuice", 4), (3, "!orangejuice", 5)], 1)

juiceLabels :: [String]
juiceLabels = ["!applejuice", "!orangejuice"]

order :: LTS
order = ([0 .. 5], ["?card", "?coin", "?btn", "!snack", "!drink"],
    [ (0, "?card", 1), (1, "?btn", 2), (0, "?coin", 1), (1, "?btn", 4), (2, "!snack", 3), (4, "!drink", 5)], 0)

juiceDispenser :: [Label] -> Bool
juiceDispenser trace = matchOutputs trace juiceLabels juiceImpl

problem4 = juiceDispenser ["?apple", "!applejuice"]

-- == Problem 5 ==
corona r s x0 t = r^t*x0 + s * r^t/r