{-# LANGUAGE LambdaCase #-}
module Exercise5 where

import LTS
import Exercise4(after)
import Exercise3 (ioTraces)
import qualified Debug.Trace as Debug
import Data.List ( find, nub )
import Data.Maybe (listToMaybe)
import Control.Exception (try, SomeException)
import Control.Exception.Base (evaluate)
import System.IO.Unsafe (unsafePerformIO)


type Impl = State -> Label -> (State, Label)

-- Time Spent: 600 min

-- (This was a bit of a mess, because we tried very hard to test everything with the implementation itself.
-- This involved writing a seperate after and out and we ran into a lot of issues, because you cant see all
-- the possible inputs and outputs of a function)

{-
## Strategy
Ultimately after starting over twice the strategy became:
1. Generate an IOLTS from the implementation
2. Get all the traces for the model and get the out after these traces
    (to keep it simple we did not use straces as our version of straces function is infinite and this does not give clear errors for each implementation )
3. Run out on the after for all the traces on the IOLTS from the impl and check for differences.


First we look at each of the implementations to see if we can find issues
without running the tests:
1. This one is correct by definition (we create IOLTS model based on its behaviour)
2. This door starts in the opened state q_0=closed
3. Unlock does not change state to unlocked, which means door cannot unlock
4. unlock/lock commands are swapped (unlock locks the door and vice versa)
5. opening does not return the agreed 'opened' statement
6. Is a very inefficient version of the correct model. (duplicated states).
    Fails because after closing and opening 3 times you cant close again (you can reset by locking)
7. When in state 6 you cannot unlock ever, because it throws an error.
8. after trace  ["closed","locked","unlocked","opened","closed","opened","closed"]


## Descriptive errors and pretty printing
The initial function along the description in the exercise we came up with was:
```
oco :: IOLTS -> Impl -> Bool 
ioco model impl = do
    let implModel = implToIOLTS implStates implLabels impl
    and
        [ 
          i `elem` (out model (model `after` traces))
        | traces <- take 10000 $ ioTraces model
        , i     <- out implModel (implModel `after` traces)
        ]
```
To get more descriptive errors we used the Either monad to return True if passed or otherwise a relevant Error.
    We decided the most useful information is the trace from model that invalidates the implementation
    as well as the difference in out values between model and implementation
Creating these functions were a lot of work, but taught is a lot about how to propagate Either and only print
errors at the end.
-}

-- doorModel :: IOLTS
-- doorModel = createIOLTS [
--                 (0,"?close", 1),
--                 (1,"!closed",2),
--                 (2,"?open", 3),
--                 (3,"!opened",0),
--                 (2, "?lock", 4),
--                 (4,"!locked",5),
--                 (5, "?unlock", 6),
--                 (6,"!unlocked",2)]

-- First we used the model above, but this meant the states did not line up with the implementations anymore
-- others suggested modeling the input and output labels on the same transitions, which made dynamically generation IOLTSs possible.
doorModel :: IOLTS
doorModel = createIOLTS [
                (0,"?close", 1),
                (0,"!closed",1),
                (1,"?open", 0),
                (1,"!opened",0),
                (1, "?lock", 2),
                (1,"!locked",2),
                (2, "?unlock", 1),
                (2,"!unlocked",1)]

-- Dynamically create an IOLTS from an implementation (Very chuffed about this one!)
implToIOLTS :: [State] -> [Label] -> Impl -> IOLTS
implToIOLTS qs l_I impl = createIOLTS getTransitions
    where
        getTransitions = concat [
            [(q, "?" ++ l_in, q_next), (q, "!" ++ l_out,q_next )] |
                (q, l_in) <- getValidInputs qs l_I impl, let (q_next, l_out) = impl q l_in]

-- Function that goes through all states and input labels and check whether this results in an error
-- returns all valid input combinations (state, label)
-- As we do not do anything with the IO monad apart from catching errors we are allowed to use unsafePerformIO
-- Used: https://stackoverflow.com/questions/3642793/why-can-haskell-exceptions-only-be-caught-inside-the-io-monad
getValidInputs :: [State] -> [Label] -> Impl -> [(State, Label)]
getValidInputs qs l_I impl = [ (q, l) | q <- qs, l <- l_I,
    let Right res = safeImpl q l, isRight (safeImpl q l)] where
        safeImpl s l = unsafePerformIO $ try (evaluate (impl s l)) >>= \case
            Right result -> return $ Right result
            Left (_ :: SomeException) -> return $ Left ("Error in impl function")
        isRight (Right _) = True
        isRight _ = False

-- Out function from the description by Tretmans
out :: IOLTS -> [State] -> [Label]
out  _ [] = [delta]
out (_, _, l_U, lt, _) qs =
    filter (`elem` l_U) (nub outputLabels)
    where
        outputLabels = concat [stateOutputLabels q | q <- qs]
        stateOutputLabels q = map snd $ nextTransitions' lt q

-- ioco function which either results in True or an error stating the trace that invalidates the impl
ioco :: IOLTS -> Impl -> Either String Bool 
ioco model impl = do
    let implModel = implToIOLTS implStates implLabels impl -- Dynamically convert impl to IOLTS to compare
    checkTraces model implModel (take nTraces $ ioTraces model)

-- Helper functions for ioco which check individual traces
findInvalidTrace :: IOLTS -> IOLTS -> [Trace] -> Maybe (Trace, Label, [Label])
findInvalidTrace model implModel traces = 
    find isInvalid invalidTraces
    where
        invalidTraces = [(trace, modelOuts, expectedOuts trace) | trace <- traces, modelOuts <- out implModel (implModel `after` trace)]
        isInvalid (trace, modelOuts, expectedOuts) = not (modelOuts `elem` expectedOuts)
        expectedOuts trace = out model (model `after` trace)

checkTraces :: IOLTS -> IOLTS -> [Trace] -> Either String Bool
checkTraces model implModel traces = 
    case findInvalidTrace model implModel traces of
        Nothing -> Right True
        Just (trace, outputs, expected) -> Left $ "Invalid for trace: " ++ show trace ++ "\n\t\tbecause: " ++ show outputs ++ " was not in output options: " ++ show expected

-- ### Hard coded states and labels to check in implementation
-- Needed to get IOLTS models from implementation
implStates :: [State]
implStates = [0..8] -- max state of any of these implementations is 7

implLabels :: [String]
implLabels = ["close", "open", "lock", "unlock"]

implementations :: [[Char]]
implementations = ["doorImpl" ++ show x | x <- [1..8]]

-- Since a lot of the implementations have traces that keep looping, we decided to take a set number.
nTraces :: Int
nTraces = 1000 -- Arbitrarily decided, as this amount gives reasonable performance and we are sure all paths are walked


-- ## Section to check and pretty print results for ioco of implementations
getImplByName :: String -> Impl
getImplByName name = case name of
    "doorImpl1" -> doorImpl1
    "doorImpl2" -> doorImpl2
    "doorImpl3" -> doorImpl3
    "doorImpl4" -> doorImpl4
    "doorImpl5" -> doorImpl5
    "doorImpl6" -> doorImpl6
    "doorImpl7" -> doorImpl7
    "doorImpl8" -> doorImpl8
    _           -> error "Unknown implementation"

-- Function that checks all implementations by name and returns the name
-- combined with either True or the relevant Error
checkImplementations :: [String] -> IOLTS -> [(String, Either String Bool)]
checkImplementations impls model = map checkImpl impls
    where
        checkImpl implName = (implName, model `ioco` (getImplByName implName))


printResult :: (String, Either String Bool) -> IO ()
printResult (implName, result) = 
        case result of
            Right True -> putStrLn $ "doorModel ioco " ++ implName ++ " ?  ==> " ++ "Pass"
            Left err -> putStrLn $ "doorModel ioco " ++ implName ++ " ?  ==> " ++ "Fail\n ## Details ## => " ++ err
            _ -> putStrLn $ "Implementation " ++ implName ++ " gave: Unknown result"


-- ### Main function which checks all implementations
main :: IO ()
main = do
    let results = checkImplementations implementations doorModel
    putStrLn $ "Checking all door implementations against doorModel:"
    mapM_ printResult results


{-
CODE GRAVEYARD to illustrate fruitless attempts to directly check with impl:


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


#### Second attempt
showTraceState :: (State, Label) -> Label -> String
showTraceState node action = "\nAction from node: " ++ node ++ "\nTried action: "

runTraceSUT :: Impl -> Trace -> State -> Bool
runTraceSUT impl [] _ = True -- Base case when list of trace labels is empty 
runTraceSUT impl [l_i] q = not . null $ impl q l_i
runTraceSUT impl (l_i:l_u:trace) q
    | res_l /= l_u = error "Resulting label for impl does not align with next in trace.\n" ++ 
    | otherwise = runTraceSUT impl trace res_q
        where (res_q, res_l) = impl q l_i


testLTSAgainstSUT :: IOLTS -> Impl -> Bool
testLTSAgainstSUT model@(states, l_in, l_out, transitions, q_0) impl =
    let ts = take 100 $ ioTraces model
        initial = minimum states
    in all (\trace -> runTraceSUT impl trace initial) ts
-}