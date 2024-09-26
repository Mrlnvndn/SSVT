module Exercise3 where

import Utils

-- Time Spent: -- min (14:42)
{--
Exercise 3: 
Find the definition of the minimal property subset (MPS) in the lecture.
Implement a function that calculates the minimal property subsets,
given a 'function under test' and a set of properties.

Learnings from lecture about MPS:
How do we calculate the minimal property subsets? => 
    We filter the [p]:
        If p kills no mutants → irrelevant
        Kills same mutants as other → equivalent

So esentially we need to identify which p's kill which mutants. Perhaps by creating
a mutation table. Any prop that doesn't kill any mutants is eliminated and properties
kill the same properties we need to assess their strength and eliminate the weakest.

Considerations: 

How can we calculate the strength of a set of properties?
We calculate the mutation score as the percentage of valid mutants that are killed.
Take home questions (InfoSupport presentation will touch on this too):
● What is the interplay between code coverage and mutation score?
● What are valid mutants?
--}

{--

--}



-- Calculates Minimal Subset of Properties of a combination of a Function Under Test
-- and a set of properties
minPropSubset :: TypeFut -> [Prop] -> [Prop]
minPropSubset fut props = props