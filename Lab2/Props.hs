module Props where
import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import FitSpec(linear)

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q


-- To make everything fit together we latered the props specified in FitSpec and using our own types
prop_moduloIsZero' :: [Integer] -> Integer -> Bool
prop_moduloIsZero' mutation input = input /= 0 --> all (\v -> v `mod` input == 0) mutation

prop_linear' :: [Integer] -> Integer -> Bool
prop_linear' = linear

prop_sumIsTriangleNumberTimesInput' :: [Integer] -> Integer -> Bool
prop_sumIsTriangleNumberTimesInput' mutation input = sum mutation == sum [1 .. 10] * input

prop_firstElementIsInput' :: [Integer] -> Integer -> Bool
prop_firstElementIsInput' [] _ = False
prop_firstElementIsInput' mutation input = head mutation == input

prop_tenElements' :: [Integer] -> Integer -> Bool
prop_tenElements' mutation input = length mutation == 10

properties' = [prop_moduloIsZero', prop_linear', prop_sumIsTriangleNumberTimesInput', prop_firstElementIsInput', prop_tenElements']

--Properties map with mapping from function name to property
propMap :: PropMap
propMap = Map.fromList 
    [("prop_moduloIsZero", prop_moduloIsZero'),
    ("prop_linear", prop_linear'),
    ("prop_sumIsTriangleNumberTimesInput", prop_sumIsTriangleNumberTimesInput'),
    ("prop_firstElementIsInput", prop_firstElementIsInput'),
    ("prop_tenElements", prop_tenElements')]

