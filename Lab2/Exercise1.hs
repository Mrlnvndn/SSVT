import Data.List
import Test.QuickCheck
import Mutation

list = [10,20,30,80,25,70]

{-
We provide some mutators to mutate the output of the list in Mutation.hs. Write down which
types of output are not yet covered by these mutators, and about their weakness/strength. Come
up with a list of other mutators and implement (a subset of) them.

    Types of output that are not covered yet by the given mutators are lists that got modulated in the middle instead of
    at the beginning or end. We also didn't explore the negation of the values, or the absolute. Loads of different types
    haven't been explored yet. 
    To determine the strength of mutators, we need to look how many mutants it kills compared to other mutators. It is
    a relative measure in between mutators because there is no point of reference otherwise. 
    Not having acces to the function or list that is going to be mutated makes it difficult to determine the strength


-}


--List of possible Mutators

--Sorted List
sortedList :: [Integer] -> [Integer]
sortedList = sort

--convert to Character
convToChar :: [Integer] -> [Char]
convToChar  = map (toEnum . fromIntegral) 

--Multiply every member by 2
multByTwo :: [Integer] -> [Integer]
multByTwo = map (* 2) 

newMutators = [sortedList, multByTwo]

