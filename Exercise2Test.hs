import Data.List
import LTS
import Test.QuickCheck
import System.Random


-- type State = Integer
-- type Label = String
-- type LabeledTransition = (State, Label, State)
-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)

{-
Generate random char for the strings,
random int for the integer
but need to decided and give proof for ! or ? on the state
-}

-- Generate a random IOLTS
ltsGen :: Int -> Int -> Int -> IO IOLTS
ltsGen numStates numTransitions inputOutputRatio = do
    let states = [1..fromIntegral numStates]
    transitions <- mapM (const $ generateRandomTransition states inputOutputRatio) [1..numTransitions]
    let labels = map (\(_, l, _) -> l) transitions
    let inputLabels = nub [tail l | l <- labels, head l == '?']
    let outputLabels = nub [tail l | l <- labels, head l == '!']
    return (states, inputLabels, outputLabels, transitions, head states)

-- Generate a random transition
generateRandomTransition :: [State] -> Int -> IO LabeledTransition
generateRandomTransition states inputOutputRatio = do
    fromState <- randomChoice states
    toState <- randomChoice states
    label <- generateRandomLabel inputOutputRatio
    return (fromState, label, toState)

-- Generate a random label
generateRandomLabel :: Int -> IO Label
generateRandomLabel inputOutputRatio = do
    isInput <- randomRIO (1, inputOutputRatio)
    if isInput == 1
        then do
            inputLabel <- randomLabel
            return ('?' : inputLabel)
        else do
            outputLabel <- randomLabel
            return ('!' : outputLabel)

-- Generate a random label string
randomLabel :: IO String
randomLabel = do
    len <- randomRIO (1, 5)
    sequence $ replicate len (randomRIO ('a', 'z'))

-- Randomly choose an element from a list
randomChoice :: [a] -> IO a
randomChoice xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

-- Example usage
main :: IO ()
main = do
    iolts <- ltsGen 5 10 2  -- 5 states, 10 transitions, input/output ratio of 2
    print iolts
