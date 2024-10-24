module EXAM_UTILS.Mutation.Corona where

import Test.QuickCheck
import qualified Test.FitSpec as FS

{-
Example of mutation testing of corona
-}

type MutFut =  Double -> Double -> Double -> Integer -> Double


-- corona :: Int -> Int -> Int -> Int -> Int
-- corona r s x0 t = last.init $ scanl (\a b -> s+r*a) x0 [0..t]
corona :: MutFut
corona r s x0 t = r^t*x0 + s * r^t/r

-- Explain, why you used certain properties.
-- A bulletproof property is going with the equation or add an edge case such as 0.
-- Example Props.
prop_adheresEquation1 :: (Double, Double, Double, Integer) -> Bool
prop_adheresEquation1 (r,s,x0,t) = r^t*x0 + s * r^t/r == corona r s x0 t

-- Identity Property: For any values of r, s, x0, and t, the result of the corona function should be equal to x0 when t is 0 and s is 0
prop_Identity :: Double -> Double -> Double ->  Property
prop_Identity r s x0 = property $ corona r 0 x0 0 == x0

-- Monotonicity Property: The result should be non-decreasing as t increases.
prop_Monotonic :: Double -> Double -> Double ->  Property
prop_Monotonic r s x0 =  forAll ( choose (1, 100)) $ \t1 t2 ->
  t1 <= t2 ==> corona r s x0 t1 <= corona r s x0 t2

-- Below isnt working
-- Consistency Property: If t is 0, the result should be equal to x0. Otherwise, it should not be equal to x0.
prop_Consistency :: Double -> Double -> Double ->  Property
prop_Consistency r s x0 =  forAll ( choose (1, 100)) $ \t ->
  (corona r s x0 t == x0) == (t == 0)



-- First make sure to test the function based on 

-- Then check your properties with mutation testing. So you are testing your tests.

-- Some generators to let quickCheck pick correct values for the function
genCoronaInput::   Gen (Double, Double, Double, Integer)
genCoronaInput = do
                          g1 <-   choose (1,100)
                          g2 <-   choose (1,100)
                          r1 <-   choose (1,100)
                          r2 <-   choose (1,100)
                          return (g1,g2, r1, r2)

genNoT::   Gen (Double, Double, Double)
genNoT = do
                          g1 <-   choose (1,100)
                          g2 <-   choose (1,100)
                          r1 <-   choose (1,100)
                          return (g1,g2, r1)


-- With this equation fitspec is going to get stuck. 
-- prop: Xt+1 - Xt = (R - 1) * Xt + S
prop_recurrenceEquation :: MutFut -> Double -> Double -> Double -> Integer -> Bool
prop_recurrenceEquation func r s x0 t = (t >= 1 &&t < 2 &&  r >= 1 && r < 2 && s >= 1 && s < 2 &&  x0 >= 1 && x0 < 2)
  FS.==>
    func r s x0 (t+1) - func r s x0 t == (r-1) * func r s x0 t + s

-- Bring the X_t only on one side and inject only positive numbers, then it's working. 
prop_rewrittenEquation :: MutFut -> Double -> Double -> Double -> Integer -> Bool
-- Apparently if you do + s it get's stuck (the correct equation). if it's minus s it works. so damn funny. 
prop_rewrittenEquation  func r s x0 t =  (t >= 1  &&  r >= 1  && s >= 1  &&  x0 >= 1 )
  && ( (r /= s) && (r /= x0) && (r /= fromIntegral t)) FS.==> (func r s x0 (t+1)) == (r *  (func r s x0 t) + s)


main =
    do
      quickCheckResult $  forAll genCoronaInput prop_adheresEquation1
      quickCheck $ forAll genNoT (\(r, s, x0) -> prop_Identity r s x0)
      quickCheck $  prop_Consistency
      quickCheck $ prop_Monotonic

properties :: MutFut -> [FS.Property]
properties corona =
  [ 
    FS.property $ prop_recurrenceEquation corona

  ]

-- If something is not working, try to play around with the values. 
testFitSpec = FS.reportWith FS.args { FS.names = ["corona r s x0 t"]
                     , FS.nMutants = 10
                     , FS.nTests = 10
                     , FS.timeout = 5
                     }
                (corona :: MutFut)
                properties