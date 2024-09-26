module Utils where

import System.Random
import Data.Map
import Test.QuickCheck

-- Purely functional algorithm to randomly shuffle: https://wiki.haskell.org/Random_shuffle

-- Some useful types to make looking at these functions more enjoyable
type TypeFut = Integer -> [Integer]
type FutOut = [Integer]
type Mutant = FutOut
type Prop = Mutant -> Integer -> Bool
type Mutator = (FutOut -> Gen Mutant)
type PropMap = Map String Prop


fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)
