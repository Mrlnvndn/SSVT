module Exercise2 where
import Test.FitSpec
import FitSpec
import Exercise1
import Mutation
import Test.QuickCheck

type TypeFut = Integer -> [Integer]
type Prop = TypeFut -> Integer -> Bool
type Mutator = ([Integer] -> Gen [Integer])

countSurvivors :: Integer -> [Prop] -> TypeFut -> Integer
countSurvivors nMutants props f = 1 --implement...




survivedAll :: Mutator -> [Prop] -> TypeFut -> Integer -> Gen Bool
survivedAll mutator props f input = do
    survivedMutation <- survived mutator props f input
    return (all (==True) survivedMutation)

-- Based on mutate function in Mutation, but with different types
survived :: Mutator -> [Prop] -> TypeFut -> Integer -> Gen [Bool]
survived mutator prop fut input = mutation >>= \mutant -> mutateOrNothing2 output mutant (propertyExecutor2 prop mutant input)
        where output = fut input
              mutation = mutator output

propertyExecutor2 :: [Prop] -> [Integer] -> Integer -> Gen [Bool]
propertyExecutor2 props mutant x = return $ map (\prop -> prop mutant x) props

-- Returns the mutated result, or nothing if the result is identical to the original output
mutateOrNothing2 :: [Integer] -> [Integer] -> Gen [Bool] -> Gen [Bool]
mutateOrNothing2 output mutant res | output == mutant = return []
                                   | otherwise = res


main = do
    let nm :: Integer = 4000
    let survivedLinear = countSurvivors nm [prop_linear] multiplicationTable


    putStrLn $ "survived linear shuffled: " ++ show survivedLinear
