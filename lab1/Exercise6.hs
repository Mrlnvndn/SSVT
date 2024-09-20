import Data.List
import Test.QuickCheck
import Lecture3


{-
Your task is to write a Haskell
program for converting formulas into CNF
-}

-- data Form = Prop Name
--           | Neg  Form
--           | Cnj [Form]
--           | Dsj [Form]
--           | Impl Form Form 
--           | Equiv Form Form 
--           deriving (Eq,Ord)

-- p = Prop 1
-- q = Prop 2
-- r = Prop 3
--  -- (p -> q) <-> ((-q) -> (-p))
-- form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q)) -- (p -> q) <-> ((-p) -> (-q))
-- form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r) -- ((p -> q) && (q -> r)) -> (p -> r)

-- removeArrows :: Form -> Form
-- removeArrows (Prop p) = Prop p -- If atomic leave it
-- removeArrows (Neg f) = Neg (removeArrows f) -- Handles negation 
-- removeArrows (Cnj f) = Cnj (map removeArrows f) -- Converts any arrows to cnj
-- removeArrows (Dsj f) = Dsj (map removeArrows f)
-- removeArrows (Impl f1 f2) = Dsj [Neg (removeArrows f1), removeArrows f2]
-- removeArrows (Equiv f1 f2) = Cnj [Dsj [Neg (removeArrows f1), removeArrows f2], Dsj [removeArrows f1, Neg (removeArrows f2)]]

-- nnf :: Form -> Form
-- nnf (Prop p) = Prop p -- Base cases
-- nnf (Neg (Prop p)) = Neg (Prop p)
-- nnf (Neg (Neg (Prop p))) = Prop p -- Double negation law
-- nnf (Neg (Cnj [f1, f2])) = Dsj [Neg (nnf f1), Neg (nnf f2)]


cnf :: Form -> Form
cnf f1 =
    nnf $ arrowfree f1

-- main :: IO()
-- main = do
    -- let p = Prop 1
    -- let q = Prop 2
    -- let r = Prop 3
--     -- form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p)) -- (p -> q) <-> ((-q) -> (-p))
--     -- form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q)) -- (p -> q) <-> ((-p) -> (-q))
--     let form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r) -- ((p -> q) && (q -> r)) -> (p -> r)
--     let result = cnf form3
--     putStrLn $ "Result: " ++ show result

