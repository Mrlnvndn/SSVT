import Data.List
import System.Random
import Test.QuickCheck


data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]


{-
Matthew: Carl didn't do it, and neither did I.   
Peter: It was Matthew or it was Jack.
Jack: Matthew and Peter are both lying.
Arnold: Matthew or Peter is speaking the truth, but not both.
Carl: What Arnold says is not true.

Their class teacher now comes in. She says: three of these boys always tell the truth, and two
always lie. You can assume that what the class teacher says is true.

Use Haskell to write a function that computes who was the thief, and a function that
computes which boys made honest declarations.
-}

-- for encoding whether a boy accuses another boy.
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl    = False
accuses Matthew Matthew = False
accuses Peter Matthew   = True
accuses Peter Jack      = True
accuses Jack Matthew    = False
accuses Jack Peter      = False
accuses Arnold Matthew  = True
accuses Arnold Peter    = False
accuses Carl Arnold     = False

--Uses the statements of accuses to compute a list checking wether every boy of the list accuses someone
accusers :: Boy -> [Boy]
accusers boy = [ x | x <- boys, accuses x boy]

--to give the list of guilty boys, plus the list of boys who made honest (true) statements.
guilty, honest :: [Boy]

guilty = [boy | boy <- boys, length (accusers boy) == 1] -- Modify according to conditions

honest = [boy | boy <- boys, length (accusers boy) > 1] -- Modify based on who tells the truth

