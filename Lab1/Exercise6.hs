import Lecture3

-- Time Spent: 360 min

-- first remove the arrows, then convert to negation normal form, then apply the distribution laws and finally flatten the logical expression
cnf :: Form -> Form
cnf fs = flatten $ distlaw $ nnf $ arrowfree fs

distlaw :: Form -> Form
-- Distribute disjunctions over conjunctions
distlaw (Dsj fs) = distribute (map distlaw fs)
-- Traverse through the rest of the Form to distribute disjunctions over conjunctions
distlaw (Cnj fs) = Cnj (map distlaw fs)
distlaw (Neg fs) = Neg (distlaw fs)
distlaw (Prop f) = Prop f

distribute :: [Form] -> Form
distribute [] = Dsj []
distribute [f] = f
-- recursively go over the formulas in the disjunction
distribute (f : fs) =
  case f of
    -- if the head is a conjunction, distribute the formulas in the conjunction over the remainder of the disjunction
    Cnj gs -> Cnj (map (\g -> distlaw (Dsj (g : fs))) gs)
    -- if it is not, it will call distribute on the tail of the conjunction
    _ -> case distribute fs of
      -- if that results in a conjunction, try to distribute it from the top
      Cnj gs -> Cnj (map (\g -> distlaw (Dsj [f, g])) gs)
      -- if it is not a conjunction, create a new disjunction and if possible flatten the tail into it
      d ->
        Dsj
          ( f : case d of
              Dsj ds -> ds
              _ -> [d]
          )

flatten :: Form -> Form
flatten (Cnj fs) = Cnj (concatMap (flattenCnj . flatten) fs)
flatten (Dsj fs) = Dsj (concatMap (flattenDsj . flatten) fs)
flatten (Neg f) = Neg (flatten f)
flatten (Prop f) = Prop f

flattenCnj :: Form -> [Form]
flattenCnj (Cnj fs) = concatMap flattenCnj fs
flattenCnj f = [f]

flattenDsj :: Form -> [Form]
flattenDsj (Dsj fs) = concatMap flattenDsj fs
flattenDsj f = [f]