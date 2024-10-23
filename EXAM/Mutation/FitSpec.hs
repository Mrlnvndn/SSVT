module EXAM_UTILS.Mutation.FitSpec where


import qualified Test.FitSpec as FS
import Data.List
import Test.QuickCheck (Positive (Positive), getPositive)
import Data.Bits (toIntegralSized)

{-
Helper file to run FitSpec on any function.

Make sure you write properties on the 'output' of the function.




fut = sort

properties sort =
  [ property $ prop1
--   , property $ \xs -> length (sort xs) == length xs
--   , property $ \x xs -> elem x (sort xs) == elem x xs
--   , property $ \x xs -> notElem x (sort xs) == notElem x xs
--   , property $ \x xs -> head (sort (x:xs)) == minimum (x:xs)
  ]
  where
  ordered (x:y:xs) = x <= y && ordered (y:xs)
  ordered _        = True

main = reportWith args { names = ["function {args}"]
                     , nMutants = 4000
                     , nTests = 4000
                     , timeout = 0
                     }
                fut
                properties

-}