module Input_Book
where 
import Data.List 
import Data.Char 
import EFSM_2
--EFSM BOOK MODEL
transition :: [Transition]
transition = [tran0, tran1, tran2, tran3, tran4, tran5, tran6, tran7, tran8, tran9, tran10, tran11, tran12, tran13, tran14, tran15]

tran0 = Transition "t0" (S "s0") (S "s1") (Gt (Var (V "x")) (Const 0)) [I "x"] [Atrib (V "bId") (Var (V "x"))]
tran1 = Transition "t1" (S "s0") (S "s2") (Gt (Var (V "x")) (Const 0)) [I "x"] [Atrib (V "rId") (Var (V "x"))]
tran2 = Transition "t2" (S "s0") (S "s0") (Lte (Var (V "x")) (Const 0)) [I "x"] []
tran3 = Transition "t3" (S "s0") (S "s0") (Lte (Var (V "x")) (Const 0)) [I "x"] []
tran4 = Transition "t4" (S "s1") (S "s0") (Eq (Var (V "x")) (Var (V "bId"))) [I "x"] [Atrib (V "bId") (Const 0)]
tran5 = Transition "t5" (S "s1") (S "s3") (Gt (Var (V "x")) (Const 0) :&: Dif (Var (V "x")) (Var (V "bId"))) [I "x"] [Atrib (V "rId") (Var (V "x"))]
tran6 = Transition "t6" (S "s1") (S "s1") (Dif (Var (V "x")) (Var (V "bId"))) [I "x"] []
tran7 = Transition "t7" (S "s1") (S "s1") (Lte (Var (V "x")) (Const 0) :|: Eq (Var (V "x")) (Var (V "bId"))) [I "x"] []
tran8 = Transition "t8" (S "s2") (S "s1") (Eq (Var (V "x")) (Var (V "rId"))) [I "x"] [Atrib (V "bId") (Var (V "x")), Atrib (V "rId") (Const 0)]
tran9 = Transition "t9" (S "s2") (S "s0") (Eq (Var (V "x")) (Var (V "rId"))) [I "x"] [Atrib (V "rId") (Const 0)]
tran10 = Transition "t10" (S "s2") (S "s2") (Dif (Var (V "x")) (Var (V "rId"))) [I "x"] []
tran11 = Transition "t11" (S "s2") (S "s2") (Dif (Var (V "x")) (Var (V "rId"))) [I "x"] []
tran12 = Transition "t12" (S "s3") (S "s1") (Eq (Var (V "x")) (Var (V "rId"))) [I "x"] [Atrib (V "rId") (Const 0)]
tran13 = Transition "t13" (S "s3") (S "s2") (Eq (Var (V "x")) (Var (V "bId"))) [I "x"] [Atrib (V "bId") (Const 0)]
tran14 = Transition "t14" (S "s3") (S "s3") (Dif (Var (V "x")) (Var (V "rId"))) [I "x"] []
tran15 = Transition "t15" (S "s3") (S "s3") (Dif (Var (V "x")) (Var (V "bId"))) [I "x"] []

testPath = P ([tran1, tran2, tran3, tran4])

efsm :: EFSM
efsm =
    EFSM {states = [S "s0", S "s1", S "s2", S "s3"], transitions = transition, vars = [V "x", V "bId", V "rId"], start = S "s0"}