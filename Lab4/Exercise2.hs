module Lab4.Exercise2 where

import Control.Monad (replicateM)
import Data.List (intersect)
import Lab4.Exercise1
  ( prop_countable,
    prop_emptyLabelsIntersect,
    prop_transitionsExist,
  )
import Lab4.LTS (IOLTS, Label, LabeledTransition, State, createIOLTS)
import Test.QuickCheck (Gen, Property, choose, forAll, quickCheck)

-- Time spent: 120 minutes

{-
Generate random char for the strings,
random int for the integer
but need to decided and give proof for ! or ? on the state

According to Tretmans [2008] - Model-Based Testing with LTS, a valid IOLTS satisfies the following (and more):
1. Input Enabledness:
    The system should be input-enabled, meaning it must handle every possible input at every state.
2. Quiescence
    Represents a state where the system is not producing any outputs. Quiescence can be treated
    as a special output observable by the tester. The IOLTS must explicitly represent quiescence
    transitions to allow testing systems that may enter a state of waiting for further inputs
    without producing any outputs.
3. Determinism
    IOLTS are often expected to be deterministic when used in testing. This means that for
    any given input, the system should have a unique transition to a next state
4. Finite-State and Convergence
    Systems are often modeled as finite-state to make them practically testable and analyzable.
    Finite-state systems have a limited number of states and transitions, ensuring the possibility
    of running exhaustive tests.
    Strong Convergence may also be required, which ensures that the system does not engage in
    infinite sequences of internal actions (τ-actions).

However, this random IOLTS generator will be used to generate IOLTS to pass through the properties produced in Exercise1.
So, the reason for developing properties is verify that IOLTS satifies those properties. Therefore, it would not make sense
to make a random IOLTS to generate valid IOLTS's that satisfy the properties.

-}

-- type State = Integer
-- type Label = String
-- type LabeledTransition = (State, Label, State)
-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], States

{-
Loop through random list of size n
add (randInt, randLabel, randInt) and repeat for whole list
-}

-- Generate a random character
randChar :: Gen Char
randChar = choose ('a', 'z')

-- Generate a random label
generateRandomLabel :: Gen Label
generateRandomLabel = do
  isInput <- choose (1 :: Int, 2 :: Int)
  ioLabel <- randChar
  if isInput == 1
    then return ('?' : [ioLabel])
    else return ('!' : [ioLabel])

-- Generate a random state
genRandomState :: Integer -> Gen Integer
genRandomState n = choose (0, n)

-- Generate a random transition
generateTransitions :: Gen (Integer, Label, Integer)
generateTransitions = do
  n <- choose (0, 10)
  fromState <- genRandomState n
  label <- generateRandomLabel
  toState <- genRandomState n
  return (fromState, label, toState)

-- Generate a random IOLTS
ltsGen :: Gen IOLTS
ltsGen = do
  len <- choose (1, 20) -- Length of the list of transitions
  transitions <- replicateM len generateTransitions
  return $ createIOLTS transitions

main :: IO ()
main = do
  quickCheck $ forAll ltsGen (prop_emptyLabelsIntersect :: IOLTS -> Property)
  quickCheck $ forAll ltsGen (prop_countable :: IOLTS -> Property)
  quickCheck $ forAll ltsGen (prop_transitionsExist :: IOLTS -> Property)
