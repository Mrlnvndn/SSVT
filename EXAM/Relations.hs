module EXAM.Relations where
import Test.QuickCheck
import Data.List


type Rel a = [(a, a)]
type RelFunc a = (Eq a => Rel a -> Bool)
-- Website with relations: https://www.ics.uci.edu/~alspaugh/cls/shr/relation.html

-- === COMPARISON ===

-- subsets :: [a] -> [[a]]
-- subsets [] = [[]]
-- subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)


comparePropsOnDomain :: Ord a => [a] -> RelFunc a -> RelFunc a -> String
comparePropsOnDomain domain prop1 prop2 = do
  if prop1Stronger && prop2Stronger then "equivalent"
     else if prop1Stronger then "prop1 is stronger"
     else if prop2Stronger then "prop2 is stronger"
     else "incomparable"
  where
    inProperty prop = filter prop relations
    relations = subsequences $ allRelationsDomain domain
    prop1Stronger = all (\r -> (prop1 r) --> (prop2 r)) (inProperty prop1)
    prop2Stronger = all (\r -> (prop2 r) --> (prop1 r)) (inProperty prop2)


-- From Lecture 2
compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q 
                    qp = stronger xs q p 
                in 
                  if pq && qp then "equivalent"
                  else if pq  then "prop1 stronger"
                  else if qp  then "weaker"
                  else             "incomparable"


stronger, weaker :: [a] -> 
       (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall' xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 


forall' :: [a] -> (a -> Bool) -> Bool
forall' = flip all

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- === PRODUCT ===

-- Used a very nice monadic way to get cartesian products from:
--  https://stackoverflow.com/questions/4119730/cartesian-product-of-2-lists-in-haskell
cartesianProduct :: (Eq a, Ord a) => [a] -> [a] -> Rel a
cartesianProduct xs ys = (,) <$> xs <*> ys

allRelationsDomain domain = cartesianProduct domain domain

getReflexive :: (Ord a) => [a] -> Rel a
getReflexive domain = map (\x -> (x, x)) domain


-- === RELATIONS ===


domainFromRel :: (Eq a) => Rel a -> [a]
domainFromRel r = map fst r `union` map snd r

-- deltaA: creates a relation where every element is related to itself
-- Also called the identity relation
deltaA :: (Eq a) => [a] -> Rel a
deltaA xs = [(x, x) | x <- xs]

refClos :: (Eq a) => Rel a -> [a] -> Rel a
refClos r domain = r `union` deltaA domain

isReflexive :: (Eq a) => Rel a -> Bool
isReflexive r = all (\x -> (x, x) `elem` r) (domainFromRel r)

isIrreflexive :: (Eq a) => Rel a -> Bool
isIrreflexive r = not $ any (\x -> (x, x) `elem` r) (domainFromRel r)

isCoreflexive :: (Eq a) => Rel a -> Bool
isCoreflexive = all (\(x, y) -> x == y)

isSymmetric :: (Eq a) => Rel a -> Bool
isSymmetric r = all (\(x, y) -> (y, x) `elem` r) r

isAntisymmetric :: (Eq a) => Rel a -> Bool
isAntisymmetric r = all (\(x, y) -> (y, x) `elem` r --> x == y) r

isAsymmetric :: (Eq a) => Rel a -> Bool
isAsymmetric r = isIrreflexive r && isAntisymmetric r

inverse :: (Eq a) => Rel a -> Rel a
inverse r = [(y, x) | (x, y) <- r]

symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ nub $ r ++ inverse r

isTransitive :: (Eq a) => Rel a -> Bool
isTransitive r = and [(x, z) `elem` r | (x, y) <- r, (y', z) <- r, y == y']

isAntiTransitive :: Eq a => Rel a -> Bool
isAntiTransitive x =
  not $ or [ (a, d) `elem` x | (a, b) <- x, (c, d) <- x, b == c ]

isPartialOrder :: Eq a => Rel a -> Bool
isPartialOrder r = isTransitive r && isReflexive r && isAntisymmetric r

isCoReflexive :: Eq a => Rel a -> Bool
isCoReflexive r = all (\(x,y) -> x == y) r

isQuasiReflexive :: Eq a => Rel a -> Bool
isQuasiReflexive r = and [ (x, x) `elem` r && (y, y) `elem` r | (x, y) <- r ]

isEquivalent :: Eq a => Rel a -> Bool
isEquivalent r = isReflexive r && isSymmetric r && isTransitive r

isLinear :: Eq a => Rel a -> Bool
isLinear r = or [ (x, y) `elem` r || (y, x) `elem` r || x == y | (x, y) <- r ]



-- === Changing relations around ===
compose :: Eq a => Rel a -> Rel a -> Rel a
compose r1 r2 =
  concatMap (\(x, y) -> concatMap (\(x2, y2) -> [ (x, y2) | y == x2 ]) r2) r1

squareCompose :: Eq a => Rel a -> Rel a
squareCompose r =
  concatMap (\(x, y) -> concatMap (\(x2, y2) -> [ (x, y2) | y == x2 ]) r) r

inverseRel :: Rel a -> Rel a
inverseRel = map (\(x, y) -> (y, x))

inverseCompose :: Eq a => Rel a -> Rel a
inverseCompose r = compose r inv where inv = inverseRel r



-- === GENERATORS ===
generateReflexiveRel :: Gen (Rel Int, [Int])
generateReflexiveRel = do
  lengthOfR <- chooseInt (1, 20)
  listOfT   <- vectorOf lengthOfR genEqTuple
  let unique = nub listOfT
  let domain = map fst unique
  return (listOfT, domain)

generateQReflexiveRel :: Gen (Rel Int, [Int])
generateQReflexiveRel = do
  (reflexiveR, domain) <- generateReflexiveRel
  newAddition          <- chooseInt (1, 10000)
  let newDomain = newAddition : domain
  return (reflexiveR, newDomain)

generateSymmetricRel :: Gen [(Int, Int)]
generateSymmetricRel = do
  length         <- chooseInt (1, 30)
  randomRelation <- vectorOf length genRandomTuple
  return (addSymmetric randomRelation randomRelation)
  where 
    addSymmetric [] output = output
    addSymmetric ((x, y) : xs) output
      | (y, x) `elem` output = addSymmetric xs output
      | otherwise            = addSymmetric xs ((y, x) : output)

genEqTuple :: Gen (Int, Int)
genEqTuple = do
  i <- chooseInt (1, 20)
  return (i, i)

genRandomTuple :: Gen (Int, Int)
genRandomTuple = do
  i <- chooseInt (1, 2000)
  y <- chooseInt (1, 2000)
  return (i, y)

genEqTupleWI :: Rel Int -> Gen (Rel Int)
genEqTupleWI randomRel = do
  return (concatMap (\(x, y) -> [(x, x), (y, y)]) randomRel)