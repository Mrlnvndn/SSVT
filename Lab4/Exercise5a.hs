module Exercise5a where

import LTS
import Exercise4(after)
import Exercise3 (ioTraces)
import qualified Debug.Trace as Debug
import Data.List
import Data.Maybe (listToMaybe)
import Control.Exception (try, SomeException)
import Control.Exception.Base (evaluate)
import System.IO.Unsafe (unsafePerformIO)


doorImplCorrect = doorImpl1

{-
Ex 5: Analyze smartdoor implementations

## Comparing 'after' traces for impl and model. From the assignment it seems we need to test
the SUT implementation directly with a model. This raises some issues:
- Some output states we would have in our iolts model, but these do not exist as an input
in the impl.
- We need two seperate 'after' functions, which also have to be properly tested.
- Some of the implementations are only wrong when we consider the output label they return
    It is quite tricky to test this and encode this in the IOLTS model



-- Create an IOLTS, which models the correct door behaviour.
-}
type Impl = State -> Label -> (State, Label)


-- doorImpl1 :: State -> Label -> (State, Label)
-- doorImpl1 0 "close" = (1, "closed")
-- doorImpl1 1 "open" = (0, "opened")
-- doorImpl1 1 "lock" = (2, "locked")
-- doorImpl1 2 "unlock" = (1, "unlocked")
-- doorImpl1 _ _ = error "Invalid command and/or state!"

doorModel :: IOLTS
doorModel = createIOLTS [
                (0,"?close", 1),
                (1,"!closed",2),
                (2,"?open", 3),
                (3,"!opened",0),
                (2, "?lock", 4),
                (4,"!locked",5),
                (5, "?unlock", 6),
                (6,"!unlocked",2)]

-- ImplToIOLTS :: State -> Label -> (State, Label) ->
-- ImplToIOLTS impl labels states = 

-- Create a version of after that calculates the states for an implementation
-- This works similar to the after function in Ex4, except for the fact an implementation
-- will never have multiple outputs for the same Trace so our result list will always be 1 long.
afterImpl :: Impl -> Trace -> (State, Label) -> [Label]
afterImpl impl trace initState = [stepAfterImpl impl trace initState]

stepAfterImpl :: Impl -> Trace -> (State, Label) -> Label
stepAfterImpl impl [] prev = snd prev -- Base case when list of trace labels is empty 
stepAfterImpl impl [inputLabel] prev = inputLabel
stepAfterImpl impl (inputLabel:outputLabel:trace) prev
    | snd nextState /= outputLabel = error $ "Output impl: "
        ++ show (nextState) ++ "\nNot equal to: " ++ show (outputLabel) ++
        " after step " ++ show prev
    | otherwise = stepAfterImpl impl trace nextState
    where
        nextState = impl (fst prev) inputLabel

stateLabels :: IOLTS -> [State] -> [Label]
stateLabels (states, _, _, transitions, _) stateList
    | any (\s -> not (s `elem` states)) stateList = error "State not in IOLTS states"
    | otherwise = [l | (sFrom, l, sTo) <- transitions, sTo `elem` stateList]

stateLabelsImpl :: Impl -> [State] -> [State] -> [Label] -> [(State, Label)]
stateLabelsImpl impl states allStates allLabels = [(s,l) | (s,l) <- implRes, s `elem` states]
    where
        implRes = [catchImpl s l | s <- allStates, l <- allLabels, isValidState s, isValidLabel l]
        isValidState s = s `elem` allStates
        isValidLabel l = l `elem` allLabels
        catchImpl s l = unsafePerformIO $ do
            result <- try (evaluate (impl s l)) :: IO (Either SomeException (State, Label))
            return $ case result of
                Right res -> res
                Left _ -> (-1, "Error in impl function")

ioco :: Impl -> IOLTS -> Bool
ioco impl model@(states, l_in, l_out, transitions, q_0) =
    let ts = take 100 $ ioTraces model
        initial = case listToMaybe (stateLabelsImpl impl [0] states (l_in `union` l_out)) of
                    Just x  -> x
                    Nothing -> error "Empty list: stateLabelsImpl returned an empty list"
    in and [ 
            Debug.trace ("Trace: " ++ show trace) $
             Debug.trace ("after impl: " ++ show (afterImpl impl trace initial)) $
             Debug.trace ("after model: " ++ show (zip (model `after` trace) (stateLabels model $ model `after` trace))) $
             (afterImpl impl trace initial) == (stateLabels model $ model `after` trace)
           | trace <- ts ]


testLTSAgainstSUT :: IOLTS -> Impl -> Bool
testLTSAgainstSUT model impl = impl `ioco` model


main :: Bool
main = do
    testLTSAgainstSUT doorModel doorImpl1