module Exercise3 where

import LTS
import Data.List
import qualified Data.Map as Map
import Test.QuickCheck
import Control.Monad (liftM2, join)
import Exercise2(ltsGen)
import Data.Maybe (fromMaybe)
import Exercise2 (ltsGen)


{-
Exercise 3: Create and test a function to get straces

The difficulty of finding these traces was of course the delta transition
quickly makes straces infinitely large. Therefore the result will either
be some infinite list of straces or we have to allow ourselves to stop at some
point when we have too many delta-transitions.

It seemed the easiest to us to first get all traces and then afterwards add the
straces inbetween, since they don't change the possible paths at all.
Delta could be seen as a sort of no-op or skip in our execution, which can
occur in any state, but functionally nothing is changed.

We do this by basically creating a strace generator, which starts with the list
of traces and keeps inserting delta's at a random index. It does this first
for each Trace in the list (otherwise we will only ever expand on the first trace).

This allows us to take n from this generator and when n is not much larger than
the amount of normal traces, we will get reasonable straces with only a few
delta transitions added in randomly. We thought this might be a useful tool
to test these paths without endlessly looping noops.
-}



-- A mapping from state to all its possible branches
transMap :: [State] -> [LabeledTransition] -> Map.Map State [LabeledTransition]
transMap states lt = Map.fromList [(q, findTransitions q) | q <- states]
    where
        findTransitions q = [t | t@(s, _, _) <- lt, s == q]


ioTraces :: IOLTS -> [Trace]
ioTraces (_, _, _, [], _) = []
ioTraces (_, _, _, lt, q_0) = nub $ map snd (traces' lt [([q_0],[])])


insertAt :: a -> Int -> [a] -> [a]
insertAt el _ [] = [el]
insertAt el i (x:xs)
    | i < 0 = error "Index cannot be negative"
    | i == 0 = el:x:xs
    | otherwise = x : insertAt el (i - 1) xs

last' :: [a] -> a
last' ys = foldl1 (\_ -> \x -> x) ys

insertRandomDeltas :: [Trace] -> [Label] -> [Label] -> Gen [Trace]
insertRandomDeltas ts inLabel outLabel = do
    afterInsert <- mapM withRandomDelta ts
    liftM2 (++) (return afterInsert) (insertRandomDeltas afterInsert inLabel outLabel)
    where
        withRandomDelta trace = do
            let maxIndex = length trace -- - if last' trace == tau then 1 else 0
            randIndex <- choose (-10, maxIndex)
            {-
            This if statement is an attempt to only insert deltas when they are suppose to be there
            meaning, only at states that have no output.
            However this does not fully work
            The logic to is to check if randIndex is less than or equal to the index of the input states
            if it is, then insert delta before
            And if randIndex > the action state then insert delta.
            -}
            if any (`elem` trace) inLabel && 
                any (\state -> randIndex <= fromMaybe 100 (elemIndex state trace)) inLabel &&
                any (\state -> randIndex > fromMaybe 100 (elemIndex state trace)) outLabel
                then return $ insertAt delta randIndex trace
            else return trace

straces :: IOLTS -> Gen [Trace]
straces model@(states, inLabels, outLabels, _, initialState) = do
    let ts = ioTraces model
    deltas <- insertRandomDeltas ts inLabels outLabels
    return (ts ++ deltas)


genRandomStraces :: Gen [Trace]
genRandomStraces = join $ straces <$> ltsGen

exIOLTS :: IOLTS
exIOLTS = createIOLTS [(0, "?a", 1), (0, "?c", 5), (1, "!x", 2), (0, "?b", 3), (3, "!y", 4), (5,"!w", 6)]

-- Property: The generated traces list should not be empty
prop_nonEmptyTraces :: IOLTS -> Property
prop_nonEmptyTraces model = forAll (straces model) $ \traces ->
    not (null traces)

-- Property: The length of the generated traces list should be greater than or equal to the length of the original traces list
prop_lengthTraces :: IOLTS -> Property
prop_lengthTraces model = forAll (straces model) $ \traces ->
    length (take 200 traces) >= length (take 200 (ioTraces model))


main = do
    -- res <- generate (straces exIOLTS)
    -- let result = take 300 res
    -- putStrLn "Generated first straces for exIOLTS:"
    -- print result
    quickCheck (forAll ltsGen prop_nonEmptyTraces)
    quickCheck (forAll ltsGen prop_lengthTraces)