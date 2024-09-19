import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


  


{-
Time spent: 120 Minutes
    We want to tranform formulas in a CNF form. To do so, we need to look at the expression and break it up into
    clauses (AND or OR) and Literals (Variables). For this particular form we could also look into all the FALSE results of 
    a truth table, take all its components, negate them and "add" them up together with "OR" and their negated form. Repeat the step
    with every False line by adding them with "AND"
    The whole exercise could have been done using a lexer (to tokenize specific cases) and a parser. It would have
    been clearer and more efficient, but the time limitation hinders us to do such an integration, so we'll stick
    to identifying cases manually.

    The steps are the following:
    - Removal of implications and equivalence using De Morgan's law
    - Moving the negation inside the expressions
    - Distribute Disjunction on Conjonction

    The printed results are correct, however multiple same expression can be printed. The program could be 
    enhanced by recognizing which expressions are the same and just print 1 of them

-}




-- Calling all the following functions
cnf :: Form -> Form
cnf = conv . moveNeg . removal 

--Removal of Implications and Equivalences
removal :: Form -> Form
removal (Prop x) = Prop x -- Nothing to change
removal (Neg x) = Neg(removal x) -- Will return the negation of a property with a value that will get assigned to it
removal (Cnj xs) = Cnj(map removal xs) -- Same Process with mapping on every simple element in the list
removal (Dsj xs) = Dsj (map removal xs )
removal (Impl f1 f2) = Dsj[Neg(removal f1),removal f2] -- Apply De morgan's Law (A -> B becomes ~A V B)
removal (Equiv f1 f2) = Cnj[removal(Impl f1 f2),removal(Impl f2 f1)] --Apply De Morgan's Law (A <-> B becomes (A->B)AND(B->A)), Implication is going to get deconstructed in previous step

--Convert Expressions with negations in front of it and other rules
moveNeg :: Form -> Form
moveNeg (Prop x) = Prop x
moveNeg (Neg (Prop x)) = Neg (Prop x) --Apply negation to normal prop
moveNeg (Neg(Neg x)) = moveNeg x -- Double negation annulation
moveNeg (Neg(Cnj xs)) = Dsj (map (moveNeg . Neg) xs) -- ~(A'AND'B) becomes ~A V ~B
moveNeg (Neg(Dsj xs)) = Cnj (map (moveNeg . Neg) xs) -- ~(A V B) becomes ~A 'AND ~B
moveNeg (Cnj xs) = Cnj (map moveNeg xs) --Apply negation inside every element
moveNeg (Dsj xs) = Dsj (map moveNeg xs)

--Distribute Disjunction over Conjunction
conv :: Form -> Form -- A V (C 'AND' B) becomes (A V B) 'AND' (A V C)
conv (Cnj fs) = Cnj (map conv fs) -- Nothing to do expect mapping
conv (Dsj [f1, f2]) = 
    case (conv f1, conv f2) of
        (Cnj fs1, f2') -> Cnj (map (\f -> Dsj [f, f2']) fs1)  -- A ∨ (B ∧ C) becomes (A ∨ B) ∧ (A ∨ C)
        (f1', Cnj fs2) -> Cnj (map (\f -> Dsj [f1', f]) fs2)  -- (A ∧ B) ∨ C becomes (A ∨ C) ∧ (B ∨ C)
        (f1', f2')     -> Dsj [f1', f2']  -- A ∨ B
conv (Dsj fs) = Dsj (map conv fs)
conv f = f





