
module Lab1.Exercise7 where

import Lab1.Lecture3
import Lab1.SetOrd
import Data.List
import Data.Type.Ord qualified as Gen
import Lab1.Lecture3
import Lab1.SetOrd
import System.Random
import Test.QuickCheck

-- Sub implementation from assignment
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub a = error $ "ERROR: no case for " ++ show a -- Added for debug purposes

-- Time Spent: 400 min
{--
This exercise took forever, because it was tricky to figure out what we were supposed to test for,
there were some struggles with generating forms to use with quickcheck and question 7.2 was a bit unclear to us.
But we figured it out, s
--}

{--
To prove sub works we adapted the approach from 'Haskell Road 7.7 for counting Connectives and Atoms.
This chapter proves by induction that the length of the list of subformulas
for a formula is equal to the amount of connectives + its atoms.
Changes: we added support for Impl and Equiv as well. One issue is that when we
use the same Prop (or subformula) multiple times we only count it once as a subformula.
This means our function for atomCount and conCount will be off.
First we implemented the connective count for instance as:
-- conCount :: Form -> Int
-- conCount (Prop n) = 0
-- conCount (Neg f) = 1 + conCount f
-- conCount (Cnj [f1 , f2]) = 1 + conCount f1 + conCount f2
-- conCount (Dsj [f1, f2]) = 1 + conCount f1 + conCount f2
-- conCount (Impl f1 f2) = 1 + conCount f1 + conCount f2
-- conCount (Equiv f1 f2) = 1 + conCount f1 + conCount f2

So ultimately we decided we had to collect the connectives (with their arguments) and atoms in sets and count
its length to ensure no duplicates are left. This gives us a version with sets of the same argument given in the book
meaning we can build on the same induction (only difference is subtracting duplicate props and connective expressions).
--}

extractCons :: Form -> Set Form -- This is almost 'sub' of course, but we dont count props
extractCons (Prop n) = emptySet
extractCons f@(Neg f1) = unionSet (Set [f]) (extractCons f1)
extractCons f@(Cnj [f1, f2]) = unionSet (unionSet (Set [f]) (extractCons f1)) (extractCons f2)
extractCons f@(Dsj [f1, f2]) = unionSet (unionSet (Set [f]) (extractCons f1)) (extractCons f2)
extractCons f@(Impl f1 f2) = unionSet (unionSet (Set [f]) (extractCons f1)) (extractCons f2)
extractCons f@(Equiv f1 f2) = unionSet (unionSet (Set [f]) (extractCons f1)) (extractCons f2)

extractAtoms :: Form -> Set Form -- Here we only get all atoms
extractAtoms (Prop n) = Set [Prop n]
extractAtoms (Neg f) = extractAtoms f
extractAtoms (Cnj [f1, f2]) = unionSet (extractAtoms f1) (extractAtoms f2)
extractAtoms (Dsj [f1, f2]) = unionSet (extractAtoms f1) (extractAtoms f2)
extractAtoms (Impl f1 f2) = unionSet (extractAtoms f1) (extractAtoms f2)
extractAtoms (Equiv f1 f2) = unionSet (extractAtoms f1) (extractAtoms f2)

conCount :: Form -> Int
conCount = lengthSet . extractCons

atomCount :: Form -> Int
atomCount = lengthSet . extractAtoms

-- Getting the length of a list (because that was not provided)
lengthSet :: Set a -> Int
lengthSet s = length (setToList s)
  where
    setToList (Set xs) = xs

prop_subLen :: Form -> Bool
prop_subLen f = lengthSet (sub f) == conCount f + atomCount f

{--
For other properties we looked at: https://math.stackexchange.com/questions/4502545/does-this-definition-of-subformula-in-propositional-logic-seem-correct
which shows a definition from A First Course in Logic by S. Hedman:
- identity
- negation
- union
--}
-- Check that: Any subformula of 𝐹 or 𝐺 is also a subformula of 𝐹 ∧ 𝐺
prop_hedmanUnion :: Form -> Form -> Bool
prop_hedmanUnion f1 f2 = do
  let unionForm = Cnj [f1, f2]
  unionSet (sub f1) (sub f2) `subSet` sub unionForm

-- Check that: Any subformula of 𝐹 or 𝐺 is also a subformula of 𝐹 ∧ 𝐺
prop_hedmanNegation :: Form -> Bool
prop_hedmanNegation f = sub f `subSet` sub (Neg f)

-- Check that: Any formula is a subformula of itself
prop_identity :: Form -> Bool
prop_identity f = f `inSet` sub f

{--
Implementation of 7.2: We already had a version of nsub for sets (conCount f + atomCount f),
but we did not feel this matched the description for 7.2
So we created a recursive version based on the inductive proof.
--}
nsub :: Form -> Int
nsub (Prop _) = 1
nsub (Neg f) = 1 + nsub f
nsub (Cnj [f1, f2]) = 1 + nsub f1 + nsub f2
nsub (Dsj [f1, f2]) = 1 + nsub f1 + nsub f2
nsub (Impl f1 f2) = 1 + nsub f1 + nsub f2
nsub (Equiv f1 f2) = 1 + nsub f1 + nsub f2

{--
Of course this does not work if compared to the length of the Set of subformulae,
but it does match the induction proof in Haskell Road. Therefore to test it all we have to do is
show the base case is covered and show the induction step (for each higher order order connective)
(Maybe programming this in Haskell wan't necessary, because it has been proven already, but otherwise it felt a bit light)
--}
prop_nsubBaseCase :: Name -> Bool
prop_nsubBaseCase n = nsub (Prop n) == 1

prop_nsubNegInduction :: Form -> Bool
prop_nsubNegInduction f = (nsub f + 1) == nsub (Neg f)

prop_nsubInduction :: Form -> Form -> Bool
prop_nsubInduction f1 f2 =
  (nsub f1 + nsub f2) + 1 == nsub (Cnj [f1, f2])
    && (nsub f1 + nsub f2) + 1 == nsub (Dsj [f1, f2])
    && (nsub f1 + nsub f2) + 1 == nsub (Impl f1 f2)
    && (nsub f1 + nsub f2) + 1 == nsub (Equiv f1 f2)

{--
## Testing using quickcheck:
Creating a generator for Forms was quite tricky.
I used the concept described in the reddit thread:
https://www.reddit.com/r/haskell/comments/jy8n11/help_make_custom_recursive_data_type_an_instance/
Also: https://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors was a really
cool resource to learn about <$> and <*> operators when working with monads like Gen.
Using 'sized' and 'resize' ensures we don't get formula structures that keep recursing. Also quickCheck can use this
to ramp up the complexity of formulae throughout testing.
--}

resizeFactor :: Int
resizeFactor = 4

instance Arbitrary Form where -- implementation of arbitrary for the Form data type
  arbitrary :: Gen Form
  arbitrary = sized $ \n ->
    if n <= 1
      then oneof [Prop <$> choose (1, 15), Prop <$> arbitrary] -- only choose 'leafs' of our formula if n is 1 or 0
      else
        oneof
          [ Prop <$> choose (1, 15),
            Prop <$> arbitrary,
            Neg <$> resize (n `div` resizeFactor) arbitrary,
            Cnj <$> vectorOf 2 (resize (n `div` resizeFactor) arbitrary), -- at least give conjunction 2 props / forms
            Dsj <$> vectorOf 2 (resize (n `div` resizeFactor) arbitrary), -- same for dsj
            Impl <$> resize (n `div` resizeFactor) arbitrary <*> resize (n `div` resizeFactor) arbitrary,
            Equiv <$> resize (n `div` resizeFactor) arbitrary <*> resize (n `div` resizeFactor) arbitrary
          ]

-- Generator function to get forms
formGen :: Gen Form
formGen = arbitrary :: Gen Form

main = do
  {-- Test case to understand what is going on --}
  -- let f = Dsj [ Cnj [Prop 8, Impl (Prop 12 ) (Prop 13)], Dsj [Neg (Prop 0), Neg (Prop 0)]]
  -- putStrLn ("Boolean Formula: " ++ show f)
  -- putStrLn (show (conCount f) ++ "+" ++ show (atomCount f) ++ " !== " ++ show (lengthSet (sub f)))
  -- print $ propSubLen f

  putStrLn "--- Exercise 7.1 ---"
  putStrLn "Testing identity prop:"
  quickCheck prop_identity

  putStrLn "Testing negation prop:"
  quickCheck prop_hedmanNegation

  putStrLn "Testing union prop:"
  quickCheck prop_hedmanUnion

  putStrLn "Testing sublength"
  quickCheck prop_subLen
  putStrLn "--- Exercise 7.2 ---"
  putStrLn "Testing base case"
  quickCheck prop_nsubBaseCase
  putStrLn "Testing induction step negation"
  quickCheck prop_nsubNegInduction
  putStrLn "Testing induction step rest"
  quickCheck prop_nsubInduction