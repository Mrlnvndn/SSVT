module Utils where

import System.Random
import qualified Data.Map as Map
import Test.QuickCheck
import Data.List
import Data.List (intercalate)

-- Purely functional algorithm to randomly shuffle: https://wiki.haskell.org/Random_shuffle

-- Some useful types to make looking at these functions more enjoyable
type TypeFut = Integer -> [Integer]
type FutOut = [Integer]
type Mutant = FutOut
type Prop = Mutant -> Integer -> Bool
type Mutator = (FutOut -> Gen Mutant)
type PropMap = Map.Map String Prop
type MutationMap = Map.Map String [Bool]


fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)


splitHalf :: [a] -> ([a], [a])
splitHalf l = Data.List.splitAt ((length l + 1) `div` 2) l


-- Some custom functions to neatly print the resulting Mutation Map and Equivalencies
printResMap :: Map.Map String [Bool] -> String -> IO()
printResMap resMap name = do
    putStrLn $ (show name) ++ ":"
    putStrLn . (intercalate "\n") $ mapValues $ Map.mapWithKey showEntry resMap
    putStrLn "\n"
    where
    showEntry key x = (show key) ++ ":" ++ show x

printEquivMap :: Map.Map String (Maybe String) -> String -> IO ()
printEquivMap equivMap name = do
    putStrLn $ name ++ ":"
    putStrLn . intercalate "\n" $ Prelude.map showEntry (Map.toList equivMap)
    putStrLn "\n"
  where
    showEntry (key, Just val) = "*" ++ key ++ "*" ++ " is subset of (or equivalent to) " ++ "*" ++ val ++ "*"
    showEntry (key, Nothing)  = "*" ++ key ++ "*" ++ " has no equivalent"

-- Function to get a list of the values of a Map (utility for printing)
mapValues :: Map.Map k v -> [v]
mapValues m = map (\x -> snd x) (Map.toList m)

-- Function to get a list of the values of a Map (utility for printing)
mapKeys :: Map.Map k v -> [k]
mapKeys m = map (\x -> fst x) (Map.toList m)