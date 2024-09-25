module Exercise2 where

import Control.Monad
import Data.List
import FitSpec
import Mutation
import System.Random
import Test.QuickCheck

{-
Question: what will be the input of the functionUnderTest? Where does it come from?? Currently implemented it by adding another Integer input
-}

countSurvivors :: Integer -> [(Integer -> [Integer]) -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Integer
countSurvivors numberOfMutants listOfProperties functionUnderTest = do
  mutations <- getMutants numberOfMutants functionUnderTest
  let results = [mutate' mutation listOfProperties functionUnderTest 9 | mutation <- mutations]
  return 0

getMutants :: Integer -> (Integer -> [Integer]) -> IO [[Integer] -> Gen [Integer]]
getMutants numberOfMutants functionUnderTest = do
  indices <- replicateM (fromIntegral numberOfMutants) (randomRIO (0, length mutators - 1))
  return [mutators !! i | i <- indices]

properties = [prop_moduloIsZero, prop_linear, prop_sumIsTriangleNumberTimesInput, prop_firstElementIsInput, prop_tenElements]
