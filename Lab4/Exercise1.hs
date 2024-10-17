module Exercise1 where

import LTS
import Data.List (intersect, subsequences, union)
import Test.FitSpec
import Data.Kind (Type)
import Data.Traversable

-- Time Spent: .. min

{-
Ex1: IOLTS validation -> ".. any verification is only as good as the validity of the model"

Q1: What makes an IOLTS 'invalid'

According to the definition by Tretmans et al.:
    From definition of LTS:
    1. Set of states Q is: countable and non-empty
    2. special symbol tau cannot be in Transition relation T
    3. transition relation T ⊆ Q × (L ∪ {τ }) × Q (all relations can be built from the cartesian product of
        all combinations of 2 states and all labels (+ tau).
    4. Initial state q_0 should of course be present in Q

    Additionally from definition of IOLTS
    1. L_I and L_U are 'countable sets' (p.13)
    2. L_I ∩ L_U = ∅ (empty set), meaning input 'external actions' are not also output (/system initiated) actions.

This gives us the following conditions that lead to an invalid IOLTS

Checking that states and labellists are countable is tricky, because inifinite lists are common in Haskell.
Testing for this results in the Halting Problem so we cover those cases by simply counting the length
and accepting the function will never finish if this condition is not met.

Apart from that we get conditions:
1. Set of states is empty
2. tau is in L_in
3. for any relation in T (q1, l, q2): q1 not in Q, l not in L or q2 not in Q (q1 is allowed to be equal to q2)
4. q_0 is not in Q
5. There is at least one label in both input and output set


Q2: validateLTS implementation

We wrote a function to calculate the possible transitions by creating a cartesian product between
all states and combining this with all labels in 'possibleTransitions'.
Then we wrote out all point in Q1 as clauses which result in False.

Q3: Create properties for validateLTS




-}


-- Checks countability by counting x :) (will loop forever if an infinite list is encountered)
countable ::forall (t :: Type -> Type) a. Foldable t => t a -> Bool
countable x = length x `seq` True


-- ## Main implementation of model validation ##
cartProd :: Ord a => [a] -> [a] -> [(a,a)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

possibleTransitions :: [State] -> [Label] -> [LabeledTransition]
possibleTransitions states labels = [(fst stateTrans, label, snd stateTrans)
                | stateTrans <- cartProd states states, label <- labels]


validateLTS :: IOLTS -> Bool
validateLTS ([], _, _, _, _) = False
validateLTS (states, l_in, l_out, transitions, q_0)
    | (not (countable states) || not (countable l_in) || not (countable l_out) ) = False
    | (not $ q_0 `elem` states) = False
    | tau `elem` l_in = False
    | l_in `intersect` l_out /= [] = False
    | any (\t -> not $ transitionPossible t states (l_in `union` l_out ++ [tau, delta])) transitions = False
    | otherwise = True
        where
        transitionPossible transition states labels = 
            transition `elem` (possibleTransitions states labels)


-- Helper function to defines how truth value of property and a validation should relate
-- We cannot just do ==, because there could be another part of IOLTS that invalidates the model.
holdsForProp :: IOLTS -> Bool -> Bool
holdsForProp model prop = validation ==> prop && not prop ==> not validation
    where validation = validateLTS model

-- ## Properties ##
prop_emptyLabelsIntersect :: IOLTS -> Property
prop_emptyLabelsIntersect model@(_, l_in, l_out, _, _) = property $
    holdsForProp model (null $ l_in `intersect` l_out)

prop_countable :: IOLTS -> Property
prop_countable model@(states, l_in, l_out, _, _) = property $ 
    holdsForProp model $ countable states && countable l_in && countable l_out

prop_transitionsExist :: IOLTS -> Property
prop_transitionsExist model@(states, l_in, l_out, transitions, _) = property $
    holdsForProp model transitionsExist
    where
        transitionsExist =
            all
            (\t -> t `elem` (possibleTransitions states (l_in `union` l_out ++ [tau, delta])))
            transitions



-- Make list of IOLTS models in LTS.hs
ltsList :: [IOLTS]
ltsList = [ counterImpl, counterModel, coffeeImpl1, coffeeModel1, coffeeImpl2, coffeeModel2, coffeeImpl3,
    coffeeModel3, coffeeImpl4, coffeeModel4, coffeeImpl5, coffeeModel5, coffeeImpl6,coffeeModel6,
    tretmanK2,tretmanK3,
    tretmanI1,tretmanI2,tretmanI3,tretmanI4,
    tretmanS1,tretmanS2,tretmanS3, tretmanS4,
    tretmanR1,tretmanR2]

main :: IO ()
main = do
    -- Validate all iolts models
    mapM_ putStrLn (map (\lts -> if validateLTS lts then "Validated" else "Invalid model") ltsList)