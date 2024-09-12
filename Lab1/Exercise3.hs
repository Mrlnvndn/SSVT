module Exercise3 where
import Lecture2


-- Time Spent: 100 min
{- 
Exercise 3: Testing properties strength

We use the stronger and weaker functions to compare all properties from Workshop 2.1.
We first define these properties below and store them (together with their name) in propList.
We then made a custom version of quickSort that compares Prop tuples based on their function strength.

Afterwards sorting the list we check that it has correctly created a list of the properties
decreasing in strength by executing 'testDescending'.

The answer our implementation spits out is:
["Property 1","Even property","Property 4","Property 3","Property 2"]


-}

-- Property definitions
type Prop = (String, PropFn)
type PropFn = Int -> Bool

p1, p2, p3, p4 :: PropFn
p1 n = even n && n > 3
p2 n = even n || n > 3
p3 n = (even n && n > 3) || even n
p4 = p3


propList :: [Prop]
propList = [
    ("Property 1", p1), 
    ("Property 2", p2),
    ("Property 3", p3),
    ("Property 4", p4),
    ("Even property", even)]

-- Altered sorting function
quicksort' :: [Int] -> [Prop] -> [Prop]  
quicksort' _ [] = []  
quicksort' l (x:xs) = 
   quicksort' l [ a | a <- xs, stronger l (snd a) (snd x) ]  
   ++ [x]
   ++ quicksort' l [ a | a <- xs, not $ stronger l (snd a) (snd x) ]

-- Test to validate the result
testDescending :: [Prop] -> Bool
testDescending [] = True
testDescending [_] = True
testDescending (x:y:rest) = stronger [1..10] (snd x) (snd y) &&
    testDescending rest

main = do
    let l = [(-10)..10] -- Small range of integers as suggested in ex.
    let sortedProps = quicksort' l propList
    putStrLn "Order from strongest to weakest:"
    print [fst s | s <- sortedProps]

    if testDescending sortedProps
        then putStrLn "Passed descending strength test!"
        else putStrLn "Descending strength test: Failed! "

