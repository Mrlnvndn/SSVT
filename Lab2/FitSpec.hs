module FitSpec where

import Test.FitSpec
import Utils (PropMap)
import qualified Data.Map as Map

multiplicationTable :: Integer -> [Integer]
multiplicationTable x = map (* x) [1 .. 10]

-- Property 1: Output list has exactly 10 elements
prop_tenElements :: (Integer -> [Integer]) -> Integer -> Bool
prop_tenElements f x = length (f x) == 10

-- Property 1: rewritten to use the output of the fut instead of the fut
prop_tenElements' :: [Integer] -> Integer -> Bool
prop_tenElements' mutation input = length mutation == 10

-- Property 2: First number is the input
prop_firstElementIsInput :: (Integer -> [Integer]) -> Integer -> Bool
prop_firstElementIsInput f x = head (f x) == x

-- Property 2: rewritten to use the output of the fut instead of the fut
prop_firstElementIsInput' :: [Integer] -> Integer -> Bool
prop_firstElementIsInput' [] _ = False
prop_firstElementIsInput' mutation input = head mutation == input

-- Property 3: The sum of the output is the input times the 10th triangle number
prop_sumIsTriangleNumberTimesInput :: (Integer -> [Integer]) -> Integer -> Bool
prop_sumIsTriangleNumberTimesInput f x = sum (f x) == sum [1 .. 10] * x

-- Property 3: rewritten to use the output of the fut instead of the fut
prop_sumIsTriangleNumberTimesInput' :: [Integer] -> Integer -> Bool
prop_sumIsTriangleNumberTimesInput' mutation input = sum mutation == sum [1 .. 10] * input

-- Property 4: The difference between consecutive elements is the input
prop_linear :: (Integer -> [Integer]) -> Integer -> Bool
prop_linear f x = linear (f x) x

-- Property 4: rewritten to use the output of the fut instead of the fut
prop_linear' :: [Integer] -> Integer -> Bool
prop_linear' = linear

-- Property 5: Any element modulo the input is zero
prop_moduloIsZero :: (Integer -> [Integer]) -> Integer -> Bool
prop_moduloIsZero f x = x /= 0 --> all (\v -> v `mod` x == 0) (f x)

-- Property 5: rewritten to use the output of the fut instead of the fut
prop_moduloIsZero' :: [Integer] -> Integer -> Bool
prop_moduloIsZero' mutation input = input /= 0 --> all (\v -> v `mod` input == 0) mutation




properties' = [prop_moduloIsZero', prop_linear', prop_sumIsTriangleNumberTimesInput', prop_firstElementIsInput', prop_tenElements']

--Properties map with mapping from function name to property
propMap :: PropMap
propMap = Map.fromList 
    [("prop_moduloIsZero", prop_moduloIsZero'),
    ("prop_linear", prop_linear'),
    ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput'),
    ("prop_firstElementIsInput", prop_firstElementIsInput'),
    ("prop_tenElements", prop_tenElements')]

linear :: [Integer] -> Integer -> Bool
-- Added another base case to support empty lists created by the Mutators
linear [] _ = False
linear [x] _ = True
linear (x : xs) y = head xs - x == y && linear xs y

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

properties multiplicationTable =
  [ property $ prop_tenElements multiplicationTable,
    property $ prop_firstElementIsInput multiplicationTable,
    property $ prop_sumIsTriangleNumberTimesInput multiplicationTable,
    property $ prop_linear multiplicationTable,
    property $ prop_moduloIsZero multiplicationTable
  ]

testFitSpec :: IO ()
testFitSpec =
  mainWith
    args
      { names = ["multiplicationTable x"],
        nMutants = 4000,
        nTests = 4000,
        timeout = 0
      }
    (multiplicationTable :: Integer -> [Integer])
    properties