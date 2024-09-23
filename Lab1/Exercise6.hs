import Data.List
import Lecture3

-- Time Spent: 360 min

-- For this exercise, we had to write a function which takes any Form and turns it into its conjunction normal form.
-- To do this, first we have to remove the arrows for which a function already exists in Lecture3.hs called 'arrowfree'.
-- Then convert to negation normal form, we can use the function 'nnf' from Lecture3.hs, which among others uses De Morgan laws
-- Then we distribute disjunctions over conjunctions using the distribution laws recursively on the form
-- Finally we flatten and simplify the From to get to a readable form which is easily definable as a form in CNF
-- NOTE: not all functions handle all possible Form states, as some have already been rewritten at the time of calling that function

cnf :: Form -> Form
cnf fs = simplify $ flatten $ distlaw $ nnf $ arrowfree fs

-- function to apply the distribution law
distlaw :: Form -> Form
distlaw (Dsj fs) = distribute (map distlaw fs)
-- Traverse through the rest of the Form to distribute disjunctions over conjunctions
distlaw (Cnj fs) = Cnj (map distlaw fs)
distlaw (Neg fs) = Neg (distlaw fs)
distlaw (Prop f) = Prop f

-- Distribute disjunctions over conjunctions
distribute :: [Form] -> Form
distribute [] = Dsj []
distribute [f] = f
-- recursively go over the formulas in the disjunction
distribute (f : fs) =
  case f of
    -- if the head is a conjunction, distribute the formulas in the conjunction over the remainder of the disjunction
    Cnj gs -> Cnj (map (\g -> distlaw (Dsj (g : fs))) gs)
    -- if it is not, it will call distribute on the tail of the conjunction
    _ ->
      let distributedFs = distribute fs
       in case distributedFs of
            -- if distributedFs is a Conjunction, distribute its elements over f
            Cnj gs -> Cnj (map (\g -> distlaw (Dsj [f, g])) gs)
            -- if it is a Disjunction, flatten it while you are at it
            Dsj ds -> Dsj (f : ds)
            -- else, just leave it
            _ -> Dsj [f, distributedFs]

-- Simplify nested Conjunctions and Disjunctions into a single level.
flatten :: Form -> Form
flatten (Cnj fs) = Cnj (concatMap (flattenCnj . flatten) fs)
flatten (Dsj fs) = Dsj (concatMap (flattenDsj . flatten) fs)
flatten (Neg f) = Neg (flatten f)
flatten (Prop f) = Prop f

-- Flatten Conjunctions
flattenCnj :: Form -> [Form]
flattenCnj (Cnj fs) = concatMap flattenCnj fs
flattenCnj f = [f]

-- Flatten Disjunctions
flattenDsj :: Form -> [Form]
flattenDsj (Dsj fs) = concatMap flattenDsj fs
flattenDsj f = [f]

-- When there is only a Conjunction of Disjunctions left, simplify the Disjunctions by removing the duplicate Props
simplify :: Form -> Form
simplify (Cnj fs) = Cnj (map simplify fs)
simplify (Dsj fs) = Dsj (nub fs)