module Lab2.Exercise3 where

import Control.Monad
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Lab2.FitSpec
import Lab2.Mutation (mutate', mutators)
import Lab2.Utils
import System.Random
import Test.QuickCheck

-- Time Spent: 400 min
{--
Exercise 3:
Find the definition of the minimal property subset (MPS) in the lecture.
Implement a function that calculates the minimal property subsets,
given a 'function under test' and a set of properties.

Learnings from lecture about MPS:
How do we calculate the minimal property subsets? =>
    We filter the [p]:
        If p kills no mutants → irrelevant
        Kills same mutants as other → equivalent

So esentially we need to identify which p's kill which mutants and find a way to assess which props
we can drop without resulting in worse performance on killing mutants.
The tricky part is that there are a variety of ways to do this. We brainstormed a bit and figured the most
doable method would be the following:

## Our method
Perhaps we could start by creating a prop-mutation table, showing whether a prop has killed a mutant.
Any prop that doesn't kill any mutants can immediately be eliminated (to speed up the processs)
and for properties that kill the same mutants we need to assess their strength and eliminate the weakest.

# Weaker properties in relation to mutants => What does that mean?
A 'weaker' property in this context can be more easily specified as having a high kill-count.
However, we can only remove a weaker property if there are no mutants surviving because of that:
    meaning, mutants not covered in the overlap
This can get quite tricky for large amounts of properties with a lot of overlap, but we keep it
relatively simple for the sake of finishing this exercise. Therefore we only look at whether a prop is
enveloped / weakerVersionOf a different prop.
    Importantly: This means we do not eliminate all possible props. For example
    if prop1 kills mutants a and b, prop2 kills b and c and prop3 kills c and d our code would not remove prop2
    even though props 1 and 3 cover all cases without the help of 2. Neither of the combinations 1,2 or 2,3 result
    in 2 being eliminated (not completely enveloped).

So our strategy in short:

- Run each property against all mutants and store kill/survived for each combination.
- Calculate 'kill-count' / strength for each property
- Remove kill-less properties
- See if a property envelops another (same mutants + others) -> remove its 'little brother'

## Alternative strategies

Just for the sake of thinking about it more deeply, some other methods we thought about implementing:

- Running the mutation code for each subset of properties and taking the one with the least props that still
    has good mutant-killing performance (you could even design some more nuanced way to balence these goals)
    Cons: Quite an expensive way to do it. Not very elegant either
- Each cycle remove one property at random and add it again if it results in worse performance
- Ground up: Sort all properties by kill-rate and keep adding the properties together until they cover all mutants
    (or have the same result as the entire propertyset)
    [This would have been a good approach, but we only came up with this idea while writing the final code.] ->
        Still results in problems when a lot of properties are equally strong (which will happen a lot for low amount of mutations)

Some final remarks:
- Debugging this took a while before I read the output of mutate' was that False meant killed, which completely explained
    why so many mutations were 'surviving'. -_-
- We picked a small number of mutations to visualize each step in getting to a MPS. Of course results
    will differ a lot when you look at a realistic number of mutations. (No equivalencies are proven)
- In main all steps are done individually. They are grouped in minPropSubset function

--}

-- ## Generating mutations based on the 'mutators' list defined in Mutation.hs
generateRandomInteger :: IO Integer
generateRandomInteger = randomRIO (1, 100)

-- Same function as ex2
getMutations :: Integer -> IO [[Integer] -> Gen [Integer]]
getMutations numberOfMutants = do
  indices <- replicateM (fromIntegral numberOfMutants) (randomRIO (0, length mutators - 1))
  return [mutators !! i | i <- indices]

-- ## Run one property on each mutation in the list (with random input) and get the results
-- Could have just used mutate' function, but we wanted to use a Map with property names
--    for debugging and reporting purposes
runProp :: Prop -> [Mutator] -> TypeFut -> IO [Bool]
runProp prop mutations fut = forM mutations $ \mutation -> do
  randInput <- generateRandomInteger
  resForMutator <- generate (mutate' mutation [prop] fut randInput)
  return (all (== False) resForMutator)

-- Run list of properties against n mutants and create a map with property names
-- Takes the Map of Properties (like defined in Props.hs), number of Mutants to get and function under test
createPropMutantMatrix :: PropMap -> Integer -> TypeFut -> IO (Map String [Bool])
createPropMutantMatrix propMap nMutants fut = do
  mutations <- getMutations nMutants
  propResults <- mapM (\p -> runProp p mutations fut) (Map.elems propMap)
  return $ Map.fromList (zip (Map.keys propMap) propResults)

-- ## Section where we define how to look at testing equivalence / subset for properties
-- First used this simple function to test equivalence
isEquiv :: [Bool] -> [Bool] -> Bool
isEquiv xs ys = xs == ys

-- Then moved to this function to compare props by relative strength
-- Idea is that q needs to (at least) have killed all mutants where p has True
-- In this implementation equivalency of properties will also return True (resulting in p being removed)
-- Example:
--  testWeaker = do
--     let a = [False, True, False, True ]
--     let b = [False, False, False, False ]
--     putStrLn $ "Comparing a with b: " ++ show (b `isWeakerVersionOf` a)
isWeakerVersionOf :: [Bool] -> [Bool] -> Bool
p `isWeakerVersionOf` q = and $ map (uncurry (-->)) (zip p q)

-- Function to filter equivalent or enveloped (subset of another) properties out
-- Takes a MutationMap containing a mapping from prop names to their result on mutations ([Bool])
-- Returns:

-- * first the filtered Map (without equivalences)

-- * and secondly a mapping from equivalent props to their counterpart

filterEquiv :: MutationMap -> (MutationMap, Map String (Maybe String))
filterEquiv mutationMap = foldr partitionEquiv (mutationMap, Map.empty) (Map.keys mutationMap)
  where
    partitionEquiv key (filteredMap, equivMap) =
      let val = filteredMap Map.! key
          restMap = Map.delete key filteredMap
          equiv = find (\k -> val `isWeakerVersionOf` (restMap Map.! k)) (Map.keys restMap)
       in case equiv of -- Handle case if equivalence is found or not
            Just e -> (restMap, Map.insert key (Just e) equivMap)
            Nothing -> (filteredMap, equivMap)

-- Function to remove properties that do not kill any mutations
filterUseless :: MutationMap -> MutationMap
filterUseless mutationMap = Map.filter isUseless mutationMap
  where
    isUseless = not . all (== False)

-- Calculates Minimal Subset of Properties of a combination of a Function Under Test
-- and a set of properties
minPropSubset :: TypeFut -> PropMap -> Integer -> IO MutationMap
minPropSubset fut props nMutants = do
  matrix <- createPropMutantMatrix propMap nMutants multiplicationTable
  let withoutUseless = filterUseless matrix
  let (minimal, equiv) = filterEquiv withoutUseless
  return minimal

-- Main function which does the same as minPropSubset, but with some more printing for clarity
main :: IO ()
main = do
  let nMutants = 10 -- We pick quite a small amount so we can clearly see all steps
  matrix <- createPropMutantMatrix propMap nMutants multiplicationTable
  printResMap matrix "propMutantMatrix"

  let withoutUseless = filterUseless matrix
  printResMap withoutUseless "Without kill-less props remain:"

  let (filtered, equiv) = filterEquiv withoutUseless
  printResMap filtered "Final MPS after Equiv/Subset filter:"

  printEquivMap equiv "Equivalencies that were found:"
