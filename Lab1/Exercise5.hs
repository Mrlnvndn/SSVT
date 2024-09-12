module Exercise5 where
import Control.Monad


-- Time Spent: 80 min

{-
Exercise 5: CSI
We have to convert 5 statements by involved kids into logical statement and define how they relate
using Haskell functions.

The main difficulty in solving this exercise is that there are two types of statements:
1. Statements of GUILT
2. Statements of TRUTH (whether someone elses statement is truthful)

First we tried to write the statements down in shortform:
stmtM = not carl && not matthew = peter, jack or arnold
stmtP = matthew || jack = not peter, arnold and carl
stmtJ = not stmtM && not stmtP (Previous statements were BOTH wrong)
stmtA = stmtM /= stmtP (Either matthew is correct in his accusations or Peter is, but NOT BOTH)
stmtC = not stmtA (Exact opposite of stmtA -> either both matthew and peter are correct or neither are)

Statements can be turned into accusations, making an 'accusation table' for each kid:

    M   P   J   A   C
M:  0   1   1   1   0   (Not carl or I -> so accuses all others)
P:  1   0   1   0   0   (Accuses matthew and jack)
J:  0   0   0   0   1   (J = not M && not P)
A:  1   1   0   1   0   (M xor P)
C:  0   0   1   0   1   (Both M & P are speaking the truth or neither): not (M xor P)

There is only one boy, who is accused by 3 people. If 3 people have to be speaking the truth they need
to accuse the correct person, which can therefore only be Jack.

In the table above we already created close to logical formulas for the 'accuses' function for each of
the boys. We implemented each of these in by expanding the accuses function for different signatures.
Ultimately this gives the same result.

-}

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Function for every signature (boy combination), which is specified below
accuses :: Boy -> Boy -> Bool

accByM = accuses Matthew
accByP = accuses Peter
accByJ = accuses Jack
accByA = accuses Arnold
accByC = accuses Carl


-- Denoting Matthews statement: acc P/J/A = True, else false
accuses Matthew Peter = True
accuses Matthew Jack = True
accuses Matthew Arnold = True
accuses Matthew _ = False


-- Based on Peters statement (accuses matthew and jack)
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

-- Based on Jacks statement (Opposite of sections above)

accuses Jack boy = not $ Matthew `accuses` boy || Peter `accuses` boy

-- Based on Arnolds statement (Either M or P are truthful, but not both)
accuses Arnold boy = (matthewAccused || peterAccused) && not (matthewAccused && peterAccused) where
    matthewAccused = accByM boy
    peterAccused = accByP boy

-- Based on Carls statement (Both M & P are speaking the truth or neither)
accuses Carl boy = (accByM boy && accByP boy) || (accByM boy && accByP boy)


-- The teacher states that the guilty boy will have 3 accusers (as 3 speak the truth)
teacherAssertion :: Boy -> Bool
teacherAssertion guilty = length (accusers guilty) == 3

-- Three functions from the exercise that calculate the guilty and honest boys
accusers :: Boy -> [Boy]
accusers boy = [a | a <- boys, a `accuses` boy]

guilty :: [Boy]
guilty = [boy | boy <- boys, teacherAssertion boy]

honest :: [Boy]
honest = [boy | boy <- boys, checkHonesty boy] where
    checkHonesty boy = and [boy `accuses` guiltyBoy |guiltyBoy <- guilty]



-- Main function shows which boys accuse which other boys (sort of a truth table)
-- After that it prints out the guilty (should contain 1 entry) and honest (should contain 3)
main :: IO ()
main = do 
    let accTable = [(b, accusers b) | b <- boys]
    forM_ accTable $ \(b, accs) -> do
        putStrLn $ show b ++ " is accused by: " ++ show accs
    putStrLn $ "Guilty: " ++ show guilty
    putStrLn $ "Honest: " ++ show honest
    
