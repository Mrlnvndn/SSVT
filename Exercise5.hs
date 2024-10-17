import Data.List
import LTS (createIOLTS, State, Label, LabeledTransition, IOLTS)
import Test.QuickCheck
import System.Random


Testing4testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool