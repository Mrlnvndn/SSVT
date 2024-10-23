module Exam26102022
where

import Test.QuickCheck
import GHC.Integer (eqInteger)
import Data.List (nub, subsequences)
import qualified Test.FitSpec as FS
import qualified Debug.Trace as Debug

type Rel a = [(a, a)]

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

fa :: [a] -> (a -> Bool) -> Bool
fa = flip all

stronger, weaker :: [a] -> 
       (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = fa xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 


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
genDomain = nub <$> arbitrary

genRelations :: Gen [Rel Integer]
genRelations = do
    domain <- genDomain
    n <- arbitrary
    relations <- vectorOf n (genRelation domain)
    return relations

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
{-
Supporting haskell file for exam of 2022 SSVT

Timing:
- Start:15:10 
- End: 18:15 (problem 3 finished)

start again at 20:48




End of P1: 16:00

-- Things to improve:
- What is Delta_A -> read somewhere it is the symmetric relation, but cant find it
- Smallest equivalence
- Formulate invariant (3.3)

-- Questions:
1 e) -> postcondition of c and d?
1.2 -> No idea what this should be? Whether im correct
2.1 Is my answer correct. How do you test for weaker /stronger (when no overlap)
2.2 Is there a cleaner proof
3.1 Can't prove it -> seems to be correct anyway
3.2 What properties apart from just testing (only possible for very small sets)


-}

-- == Problem 2 == 


isAntisymmetric::Eq a => Rel a -> Bool
isAntisymmetric r = and [(y,x) `notElem` r || x==y |(x,y) <- r]

isIrreflexive:: Eq a => Rel a -> Bool
isIrreflexive r = not $ any (\x -> (x,x) `elem` r) (map fst r)

prop_fstStronger :: Eq a => [Rel a] -> Bool
prop_fstStronger rels = weaker rels isAntisymmetric isIrreflexive

prop_sndStronger :: Eq a => [Rel a] -> Bool
prop_sndStronger rels = stronger rels isIrreflexive isAntisymmetric

problem2_1 :: IO ()
problem2_1 = do
    quickCheck $ forAll genRelations prop_fstStronger
    quickCheck $ forAll genRelations prop_fstStronger





-- == Problem 3 ==
numR 0 = 1
numR n = 2^(2*n-2) * numR (n-1)

-- Generate the domain
generateDomain :: Int -> [Int]
generateDomain n = [1..n]

-- Generate all pairs (a_i, a_j) for i, j in the domain
generateAllPairs :: [Int] -> [(Int, Int)]
generateAllPairs domain = [(x, y) | x <- domain, y <- domain]

-- Generate the diagonal pairs (a_i, a_i)
generateDiagonalPairs :: [Int] -> [(Int, Int)]
generateDiagonalPairs domain = [(x, x) | x <- domain]

-- Generate all reflexive relations on a domain of size n
generateReflexiveRelations :: Int -> [[(Int, Int)]]
generateReflexiveRelations n = 
    let domain = generateDomain(n)
        allPairs = generateAllPairs(domain)
        diagonalPairs = generateDiagonalPairs(domain)
        optionalPairs = filter (`notElem` diagonalPairs) allPairs
    in map (diagonalPairs ++) (subsequences optionalPairs)

prop_cardinality n = length (generateReflexiveRelations n) == numR n

prop_sizeDomain :: Integral a => a -> Property
prop_sizeDomain n = property $ numR n >= n && numR n <= 2^(n^2)


genPositiveSmall :: Gen Int
genPositiveSmall = choose(0, 5)

genPositive :: Gen Int
genPositive = arbitrary `suchThat` (\x -> x > 0)

problem3 :: IO ()
problem3 = do
    quickCheck $ forAll genPositiveSmall prop_cardinality
    quickCheck $ forAll genPositive prop_sizeDomain


-- == Problem 4 ==
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type LTS = ([State], [Label], [LabeledTransition], State)
type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)
type Trace = [Label]

tau = "tau" -- Alease assume tau behaves as it is defined in the Tretmans paper 
delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper

coffeeImpl :: LTS
coffeeImpl = ([1..4], ["?btn", "?btn_milk", "!espresso", "!latte"], [(1, "?btn", 2), (1, "?btn_milk", 1), (2, "!espresso", 3), (2, "!latte", 4)], 1)
coffeeModel :: LTS
coffeeModel = ([1..5], ["?btn", "?btn_milk", "!espresso", "!latte"], [(1, "?btn", 2), (2, "!espresso", 3), (1, "?btn_milk", 4), (4,"!latte", 5)], 1)
    
drinks :: LTS
drinks = ([1..6], ["?btn", "!beer", "!cola"],[(1, "?btn", 2), (2, "!beer", 3), (1, "?btn", 4), (4, "?btn", 5), (5, "!cola", 6)], 1)

drinks' :: IOLTS
drinks' = ([1..6], ["?btn"], ["!beer", "!cola"],[(1, "?btn", 2), (2, "!beer", 3), (1, "?btn", 4), (4, "?btn", 5), (5, "!cola", 6)], 1)
-- 4.1

coffeeLabels = ["!espresso", "!latte"]

nextTransitions':: [LabeledTransition]->State->[(State,Label)]
nextTransitions' lt q0 =  [(s',l) | (s,l,s')<- lt , s == q0]

quantumCoffee :: [Label] -> Bool
quantumCoffee trace = quantumCoffee' trace initial lt
    where
        (qs, ls, lt, initial) = coffeeImpl

quantumCoffee':: [Label] -> State -> [LabeledTransition] -> Bool
quantumCoffee' [last] _ _ = last `elem` coffeeLabels
quantumCoffee' (next_l:trace) state lt
    | next_l `elem` coffeeLabels = True
    | otherwise = any (\(s', l) -> quantumCoffee' trace s' lt) nextStates
  where
    nextStates = nextTransitions' lt state


nextStates :: IOLTS -> State -> Label -> [State]
nextStates (_, _, _, transitions, _) currentState label =
    [toState | (fromState, transLabel, toState) <- transitions, fromState == currentState, transLabel == label]

-- Function to find the set of possible states after performing a trace
after :: IOLTS -> Trace -> [State]
after lts@(states, _, _, _, initialState) trace = foldl (nextStates' lts) [initialState] trace
  where
    nextStates' :: IOLTS -> [State] -> Label -> [State]
    nextStates' lts currentStates label = concatMap (\state -> nextStates lts state label) currentStates


{-
type LTS = ([State], [Label], [LabeledTransition], State)
type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

Since the after function needs an IOLTS and the traces function takes a LTS, we can pass the an LTS 
in the format of an IOLTS by adding an empty list in place of outputs.
-}
ltsToIolts :: LTS -> IOLTS
ltsToIolts (states, labels, transitions, initialState) = (states, labels, [], transitions, initialState)


problem4_2 = do
    let trace = ["?btn", "!latte"]
    putStrLn $ "Is coffe being drunk for trace: " ++ show trace ++ show (quantumCoffee trace)
    let trace = ["?btn_milk", "?btn"]
    putStrLn $ "Is coffe being drunk for trace: " ++ show trace ++ show (quantumCoffee trace)
    let trace = ["?btn_milk", "?btn", "!espresso"]
    putStrLn $ "Is coffe being drunk for trace: " ++ show trace ++ show (quantumCoffee trace)


problem4_3 = do
    putStrLn $ "a) " ++ show (after drinks' ["?btn", "!beer"])
    putStrLn $ "b) " ++ show (after drinks' ["?btn"])

-- == Problem 5 ==
corona :: Integer -> Integer -> Integer -> Integer -> Integer
corona r s x0 t = iterate ((s+).(r*)) x0 !! fromInteger t

c :: Integer -> Integer
c t = corona rPopInfections sMedInfections x0 t
-- lets put some random values
rPopInfections :: Integer
rPopInfections = 3
sMedInfections :: Integer
sMedInfections = 200
x0 :: Integer
x0 = 100

randomT :: Integer
randomT = 3

-- Custom generators for positive integers
positiveInteger :: Gen Integer
positiveInteger = arbitrary `suchThat` (> 0)

-- properties
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

prop_dayDiff :: (Integer -> Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer -> Bool
prop_dayDiff f r s x0 t = c_plusone - c_out == (r - 1) * c_out + s
  where
    c_out = f r s x0 t
    c_plusone = corona r s x0 (t + 1)

prop_communityLinear :: (Integer -> Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer -> Bool
prop_communityLinear f r s x0 t = isLinear [c (fromInteger t_i) - (sMedInfections*t_i) | t_i <- [0..t]]
  where
    isLinear xs = allTheSame $ gradient xs
    c t = f r s x0 t

-- Modified properties to use custom generators
prop_dayDiff' :: (Integer -> Integer -> Integer -> Integer -> Integer) -> Property
prop_dayDiff' f = forAll positiveInteger $ \r ->
                  forAll positiveInteger $ \s ->
                  forAll positiveInteger $ \x0 ->
                  forAll positiveInteger $ \t ->
                  prop_dayDiff f r s x0 t

prop_communityLinear' :: (Integer -> Integer -> Integer -> Integer -> Integer) -> Property
prop_communityLinear' f = forAll positiveInteger $ \r ->
                          forAll positiveInteger $ \s ->
                          forAll positiveInteger $ \x0 ->
                          forAll positiveInteger $ \t ->
                          prop_communityLinear f r s x0 t

gradient :: Num a => [a] -> [a]
gradient [x] = error "too few elements to calculate gradient"
gradient [x,y] = [y-x]
gradient (x:y:zs) = y-x : gradient (y:zs)


-- problem_5 = do
--   print prop_dayDiff
--   let cList = [c (fromInteger t_i) - (sMedInfections*t_i) | t_i <- [0..randomT]]
--   let grad = gradient cList
--   putStrLn $ "cList: " ++ show cList ++ "\nGradient: " ++ show grad
--   print $ prop_communityLinear randomT



coronaProps :: (Integer -> Integer -> Integer -> Integer -> Integer) -> [FS.Property]
coronaProps corona =
  [ FS.property $ prop_dayDiff' corona,
    FS.property $ prop_communityLinear' corona
  ]

testCorona :: IO ()
testCorona =
  FS.mainWith
    FS.args
      { FS.names = ["corona x"],
        FS.nMutants = 4000,
        FS.nTests = 4000,
        FS.timeout = 0
      }
    corona
    coronaProps