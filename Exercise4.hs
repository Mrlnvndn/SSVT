<<<<<<< Updated upstream
module Exercise4 where

import Data.List
import LTS (traces, LTS, State, Label, LabeledTransition, IOLTS, Trace)
import Test.QuickCheck
import System.Random
import Control.Monad (replicateM)
import Exercise3 (straces, ioTraces)

{-
Time spent: 140 minutes

According the Tretman, the after function is defined as: 

Let p=(Q,LI,LU,T,q0) be a labelled transition system (or input-output 
labelled transition system), where:

    Q is the set of states,
    LI is the set of input labels,
    LU is the set of output labels,
    T is the transition relation T⊆Q×(LI∪LU∪{τ})×Q,
    q0​ is the initial state.

The after function describes the set of possible states that the system can reach after performing a trace.

In this example: 
    tretmanS3 = [(0, "?a", 1), (1, "!x", 2), (0, "?b", 3), (3, "!y", 4)]
The after function would work like this:
    after tretmanS3 ["?a", "!x"] = 2
-}

nextStates :: IOLTS -> State -> Label -> [State]
nextStates (_, _, _, transitions, _) currentState label =
    [toState | (fromState, transLabel, toState) <- transitions, fromState == currentState, transLabel == label]

-- Function to find the set of possible states after performing a trace
after :: IOLTS -> Trace -> [State]
after lts@(states, _, _, _, initialState) trace = foldl (nextStates' lts) [initialState] trace
  where
    nextStates' :: IOLTS -> [State] -> Label -> [State]
    nextStates' lts currentStates label = concatMap (\state -> nextStates lts state label) currentStates


{-
type LTS = ([State], [Label], [LabeledTransition], State)
type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

Since the after function needs an IOLTS and the traces function takes a LTS, we can pass the an LTS 
in the format of an IOLTS by adding an empty list in place of outputs.
-}
ltsToIolts :: LTS -> IOLTS
ltsToIolts (states, labels, transitions, initialState) = (states, labels, [], transitions, initialState)

{-
We can test the validity of our after function implemention using the traces and straces functions.

Using the traces function, we can validate our after function by passing every trace generated from the
traces function and check that the after function never returns an empty list.
This is enough to verify the after function because the traces function returns valid traces and according 
to Tretmans a valid trace is defined as the set of all sequences of observable actions that the system 
can perform from the initial state. Therefore, if the after function is working correctly, if given a valid
IOLTS and a valid sequence of traces, then it should always be able to return a non empty list over all the
traces.

On the other side, we can use the straces function to validate our after function as well but in a different
way. The logic is the same, where we check the after function over every straces of an IOLTS. Doing this, we
expecting the output to be false because according to Tretmans, suspension traces capture not only the 
sequence of actions but also the observation of quiescence where the system produces no outputs. At these states,
we expect the after function to return an empty list. On the result of straces, we expect the after function
to sometimes return a list or an empty list when it hits a quiscence. This is reflected in the 'not' placed after
the all statement in verifyWithStraces.
-}
verifyWithTraces :: LTS -> Bool
verifyWithTraces lts@(states, inputs, transitions, initialState) =
    all (\trace -> not (null (after (ltsToIolts lts) trace))) (traces lts) 

verifyWithStraces :: IOLTS -> Gen Bool
verifyWithStraces iolts@(states, inputs, outputs, transitions, initialState) = do
    generatedTraces <- (straces exampleIOLTS)
    let list = take 1000 generatedTraces
    return $ not (all (\trace -> not (null (after iolts trace))) list)


exampleLTS :: LTS
exampleLTS = ([0, 1, 2], ["a", "b"], [(0, "a", 1), (1, "b", 2)], 0)

exampleIOLTS :: IOLTS
exampleIOLTS = ([0, 1, 2], ["?a", "?b"], ["!x", "!y"], [(0, "?a", 1), (1, "!x", 2), (0, "?b", 1), (1, "!y", 2)], 0)

main :: IO ()
main = do
    putStrLn "Testing the 'after' function with traces:"
    print $ verifyWithTraces exampleLTS
    putStrLn "Testing the 'after' function with straces:"
    result <- generate (verifyWithStraces exampleIOLTS)
    print result
=======

>>>>>>> Stashed changes
