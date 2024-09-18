import Lecture3

cnf :: Form -> Form
cnf (Equiv f1 f2) = Cnj [Dsj [Neg (cnf f1), cnf f2], Dsj [cnf f1, Neg (cnf f2)]]
cnf (Impl f1 f2) = Dsj [Neg (cnf f1), cnf f2]
cnf (Neg (Cnj [f1, f2])) = Dsj [Neg (cnf f1), Neg (cnf f2)]
cnf (Neg (Dsj [f1, f2])) = Cnj [Neg (cnf f1), Neg (cnf f2)]
cnf (Neg (Neg f1)) = cnf f1
cnf (Dsj [f1, Cnj [f2, f3]]) = Cnj [Dsj [cnf f1, cnf f2], Dsj [cnf f1, cnf f3]]
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = Dsj (map cnf fs)
cnf (Neg fs) = Neg (cnf fs)
cnf (Prop f) = Prop f