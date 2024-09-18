module Exercise3 where
import Lecture2


-- Time Spent: 100 min
{- 
Exercise 3: Testing properties strength

We use the stronger and weaker functions to compare all properties from Workshop 2.1.
We first define these properties below and store them (together with their name) in propList.
We then made a custom version of quickSort that compares and sorts Prop tuples 
based on their function strength.

After sorting the list we check that it has correctly created a list of the properties
decreasing in strength by executing 'testDescending'.

The answer our implementation spits out is:
["Property 1","Even property","Property 4","Property 3","Property 2"]

-}

-- Property definitions (with a name to allow for easy printing)
type Prop = (String, PropFn)
type PropFn = Int -> Bool

p1, p2, p3, p4 :: PropFn
p1 n = even n && n > 3 -- From first equation: (\ x -> even x && x > 3)
p2 n = even n || n > 3 -- From second equation: (\ x -> even x || x > 3)
p3 n = (even n && n > 3) || even n -- From third equation: (\ x -> (even x && x > 3) || even x)
p4 = p3 -- Right side of fourth equation is the same as p3


propList :: [Prop]
propList = [
    ("Property 1", p1), 
    ("Property 2", p2),
    ("Property 3", p3),
    ("Property 4", p4),
    ("Even property", even)] -- Also even is a property given in the exercise

-- Altered sorting function (quicksort that sorts Props)
quicksort' :: [Int] -> [Prop] -> [Prop]  
quicksort' _ [] = []  
quicksort' l (x:xs) = 
   quicksort' l [ a | a <- xs, stronger l (snd a) (snd x) ]  
   ++ [x]
   ++ quicksort' l [ a | a <- xs, not $ stronger l (snd a) (snd x) ]

-- Test to validate the result is a list of descending strength
testDescending :: [Prop] -> Bool
testDescending [] = True
testDescending [_] = True
testDescending (x:y:rest) = stronger [1..10] (snd x) (snd y) &&
    testDescending rest

main = do
    let domain = [(-10)..10] -- Small range of integers as suggested in ex.
    let sortedProps = quicksort' domain propList
    putStrLn "Order from strongest to weakest:"
    print [fst s | s <- sortedProps] -- show list with names

    if testDescending sortedProps
        then putStrLn "Passed descending strength test!"
        else putStrLn "Descending strength test: Failed! "

