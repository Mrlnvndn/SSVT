module Exercise3 where

import LTS
import Data.List
import qualified Data.Map as Map
import Test.QuickCheck
import Control.Monad (liftM2)


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

insertRandomDeltas :: [Trace] -> Gen [Trace]
insertRandomDeltas ts = do
    afterInsert <- mapM withRandomDelta ts
    liftM2 (++) (return afterInsert) (insertRandomDeltas afterInsert)
    where
        withRandomDelta trace = do
            let maxIndex = length trace -- - if last' trace == tau then 1 else 0
            randIndex <- choose (0, maxIndex)
            return $ insertAt delta randIndex trace

straces :: IOLTS -> Gen [Trace]
straces model = do
    let ts = ioTraces model
    deltas <- insertRandomDeltas ts
    return (ts ++ deltas)

main = do
    res <- generate (straces coffeeModel4)
    let result = take 30 res
    print result
