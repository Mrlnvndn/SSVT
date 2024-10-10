module Exercise2 where
import Data.List
import LTS
import Test.QuickCheck
import System.Random




{-- A separate generator for the state, just numbers. Could have a bigger range--}
ltsGenState :: Gen State -- A generator for a single state
ltsGenState = choose (0, 10)

{-- A separate generator for the labels, just chars. --}
ltsGenLabel :: Gen Label
ltsGenLabel = arbitrary

{-- A separate generator for the labeled transitions --}
ltsGenLabeledTransition :: Gen State -> Gen Label -> Gen LabeledTransition
ltsGenLabeledTransition possibleStateGen possibleLabelGen = do
    firstState <- possibleStateGen
    transitionLabel <- possibleLabelGen
    lastState <- possibleStateGen
    return (firstState, transitionLabel, if transitionLabel == delta then firstState else lastState)

ltsGen :: Gen IOLTS
ltsGen = do
    setOfPossibleStates <- listOf ltsGenState `suchThat` (not . null . nub)
    initialStateIndex <- choose (0, length setOfPossibleStates - 1)
    let initialState = setOfPossibleStates !! initialStateIndex
    setOfPossibleInputs <- nub <$> listOf ltsGenLabel -- nub, because elements in a set need to be unique 
    setOfPossibleOutputs <- filter (`notElem` setOfPossibleInputs) . nub <$> listOf ltsGenLabel
    setOfPossibleLabeledTransitions <- nub <$> listOf (ltsGenLabeledTransition (elements setOfPossibleStates) (elements (setOfPossibleInputs ++ [tau])))
    -- According to the definition we need to include tau in the inputs
    return (nub setOfPossibleStates, setOfPossibleInputs, setOfPossibleOutputs, setOfPossibleLabeledTransitions, initialState)

