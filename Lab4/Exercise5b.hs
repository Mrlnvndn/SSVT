module Exercise5b where

import LTS
import Exercise4(after)
import Exercise3 (ioTraces)
import qualified Debug.Trace as Debug
import Data.List
import Data.Maybe (listToMaybe)
import Control.Exception (try, SomeException)
import Control.Exception.Base (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_)

type Impl = State -> Label -> (State, Label)


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



main = do
    testLTSAgainstSUT doorModel doorImpl2
