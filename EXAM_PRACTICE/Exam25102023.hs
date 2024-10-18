module Exam25102023 where

-- == Problem 3 ==
numNRIr :: Int -> Int
numNRIr 0 = 0
numNRIr 1 = 0
numNRIr n = 2^(n^2-1) + 2^(2*n-2) * numNRIr (n-1)

-- == Problem 4 ==
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type LTS = ([State], [Label], [LabeledTransition], State)
tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper 
delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper

dispenserImpl :: LTS
dispenserImpl = ([1..4], ["?btn", "?switch", "!water"], [(1, "?btn", 2), (1, "?switch", 1), (1, "?btn", 3), (3, "!water", 4)], 1)

dispenserModel :: LTS
dispenserModel = ([1..4], ["?btn", "!water"], [(1, "?btn", 2), (2, "tau", 3), (2, "!water", 4)], 1)
    
order :: LTS
order = ([0..5], 
        ["?coin", "?btn", "!food", "!drink", "!receipt"],
        [
            (0, "?coin", 1),
            (1, "?coin", 2),
            (2, "!food", 3),
            (0, "?btn", 3),
            (3, "!receipt", 4),
            (0, "?coin", 5),
            (5, "!drink", 3)
        ], 0)

-- == Problem 5 ==
corona r s x0 t = r^t*x0 + s * (r^t-1)/(r-1)