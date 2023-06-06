module Input 
where 
import Data.List
import Data.Char
import EFSM_2

--EFSM EXAMPLE
--"p1" GT 10 :&: "p1" Lt 20
transition :: [Transition]
--transition = [Transition "t1" (S "s1") (S "s1") (Gte (Param "p1") (Const 10) :&: Lte (Param "p1") (Const 20) :&: Gte (Param "p2") (Const 0) :&: Lte (Param "p2") (Const 10)) [I "aa"] [Atrib (V "v1") (Param "p1"), Atrib (V "v2") (Param "p2")], Transition "t2" (S "s1") (S "s2") Nil [I "ab"] [Atrib (V "v3") (Const 10)], Transition "t3" (S "s2") (S "s3") (Gt (Var (V "v3")) (Const 0)) [I "a"] [Atrib (V "v1") (Var (V "v2") :+: Var (V "v3")), Atrib (V "v3") (Var (V "v2")) ], Transition "t4" (S "s3") (S "s1") (Gt (Var (V "v1")) (Var (V "v2"))) [I "ba"] [], Transition "t5" (S "s2") (S "s2") (Lt (Var (V "v3")) (Const 0)) [I "bb"] [Atrib (V "v1") (Const 0)], Transition "t6" (S "s3") (S "s3") (Lt (Var (V "v1")) (Param "p1") :&: Gt (Param "p1") (Param "p2")) [I "b"] []]
transition = [tran1, tran2, tran3, tran4, tran5, tran6]

tran1 = Transition "t1" (S "s1") (S "s1") (Gte (Param "p1") (Const 10) :&: Lte (Param "p1") (Const 20) :&: Gte (Param "p2") (Const 0) :&: Lte (Param "p2") (Const 10)) [I "aa"] [Atrib (V "v1") (Param "p1"), Atrib (V "v2") (Param "p2")]
tran2 = Transition "t2" (S "s1") (S "s2") Nil [I "ab"] [Atrib (V "v3") (Const 10)]
tran3 = Transition "t3" (S "s2") (S "s3") (Gt (Var (V "v3")) (Const 0)) [I "a"] [Atrib (V "v1") (Var (V "v2") :+: Var (V "v3")), Atrib (V "v3") (Var (V "v2")) ]
tran4 = Transition "t4" (S "s3") (S "s1") (Gt (Var (V "v1")) (Var (V "v2"))) [I "ba"] []
tran5 = Transition "t5" (S "s2") (S "s2") (Lt (Var (V "v3")) (Const 0)) [I "bb"] [Atrib (V "v1") (Const 0)]
tran6 = Transition "t6" (S "s3") (S "s3") (Lt (Var (V "v1")) (Param "p1") :&: Gt (Param "p1") (Param "p2")) [I "b"] []

testPath = P ([tran1, tran2, tran3, tran4])

efsm :: EFSM
efsm =
    EFSM {states = [S "s1", S "s2", S "s3"], transitions = transition, vars = [V "v1", V "v2", V "v3"], start = S "s1"}
    

-------------------------------------------------------------------
--EFSM BOOK MODEL
transition1 :: [Transition]
transition1 = [tran0, tran1, tran2, tran3, tran4, tran5, tran6, tran7, tran8, tran9, tran10, tran11, tran12, tran13, tran14, tran15]

tran0 = Transition "t0" (S "s0") (S "s1") (Gt (Var (V "x")) (Const 0)) [I "ab"] [Atrib (V "bId") (Var (V "x"))]
tran1 = Transition "t1" (S "s0") (S "s2") (Gt (Var (V "x")) (Const 0)) [I "aa"] [Atrib (V "rId") (Var (V "x"))]
tran2 = Transition "t2" (S "s0") (S "s0") (Lte (Var (V "x")) (Const 0)) [I "ab"] []
tran3 = Transition "t3" (S "s0") (S "s0") (Lte (Var (V "x")) (Const 0)) [I "a"] []
tran4 = Transition "t4" (S "s1") (S "s0") (Eq (Var (V "x")) (Var (V "bId"))) [I "ba"] [Atrib (V "bId") (Const 0)]
tran5 = Transition "t5" (S "s1") (S "s3") (Gt (Var (V "x")) (Const 0) :&: Dif (Var (V "x")) (Var (V "bId"))) [I "bb"] [Atrib (V "rId") (Var (V "x"))]
tran6 = Transition "t6" (S "s1") (S "s1") (Dif (Var (V "x")) (Var (V "bId"))) [I "b"] []
tran7 = Transition "t7" (S "s1") (S "s1") (Lte (Var (V "x")) (Const 0) :|: Eq (Var (V "x")) (Var (V "bId"))) [I "b"] []
tran8 = Transition "t8" (S "s2") (S "s1") (Eq (Var (V "x")) (Var (V "rId"))) [I "b"] [Atrib (V "bId") (Var (V "x")), Atrib (V "rId") (Const 0)]
tran9 = Transition "t9" (S "s2") (S "s0") (Eq (Var (V "x")) (Var (V "rId"))) [I "b"] [Atrib (V "rId") (Const 0)]
tran10 = Transition "t10" (S "s2") (S "s2") (Dif (Var (V "x")) (Var (V "rId"))) [I "b"] []
tran11 = Transition "t11" (S "s2") (S "s2") (Dif (Var (V "x")) (Var (V "rId"))) [I "b"] []
tran12 = Transition "t12" (S "s3") (S "s1") (Eq (Var (V "x")) (Var (V "rId"))) [I "b"] [Atrib (V "rId") (Const 0)]
tran13 = Transition "t13" (S "s3") (S "s2") (Eq (Var (V "x")) (Var (V "bId"))) [I "b"] [Atrib (V "bId") (Const 0)]
tran14 = Transition "t14" (S "s3") (S "s3") (Dif (Var (V "x")) (Var (V "rId"))) [I "b"] []
tran15 = Transition "t15" (S "s3") (S "s3") (Dif (Var (V "x")) (Var (V "bId"))) [I "b"] []

testPath1 = P ([tran1, tran2, tran3, tran4])

efsm1 :: EFSM
efsm1 =
    EFSM {states = [S "s0", S "s1", S "s2", S "s3"], transitions = transition1, vars = [V "x", V "bId", V "rId"], start = S "s0"}

-------------------------------------------------------------------
--Class2 Transfer Protocol EFSM
transition2 :: [Transition]
transition2 = [tran0, tran1, tran2, tran3, tran4, tran5, tran6, tran7, tran8, tran9, tran10, tran11, tran12, tran13, tran14, tran15, tran16, tran17, tran18, tran19, tran20]

tran0 = Transition "t0" (S "s1") (S "s2") (Nil) [I "ab"] [Atrib (V "opt") (Var (V "prop_out")), Atrib (V "R_credit") (Const 0)]
tran1 = Transition "t1" (S "s1") (S "s3") (Nil) [I "aa"] [Atrib (V "opt") (Var (V "opt_ind")), Atrib (V "S_credit") (Var (V "cr")), Atrib (V "R_credit") (Const 0)]
tran2 = Transition "t2" (S "s2") (S "s4") (Lt (Var (V "opt_ind")) (Var (V "opt"))) [I "ab"] [Atrib (V "TRsq") (Const 0), Atrib (V "TSsq") (Const 0), Atrib (V "opt") (Var (V "opt_ind")), Atrib (V "S_credit") (Var (V "cr"))]

tran3 = Transition "t3" (S "s2") (S "s5") (Gt (Var (V "opt_ind")) (Var (V "opt"))) [I "a"] []
tran4 = Transition "t4" (S "s2") (S "s1") (Nil) [] []
tran5 = Transition "t5" (S "s3") (S "s4") (Lt (Var (V "accpt_ind")) (Var (V "opt"))) [I "bb"] [Atrib (V "opt") (Var (V "accpt_opt")), Atrib (V "TRsq") (Const 0), Atrib (V "TSsq") (Const 0)]
tran6 = Transition "t6" (S "s3") (S "s6") (Nil) [] []
tran7 = Transition "t7" (S "s4") (S "s4") (Gt (Var (V "S_credit")) (Const 0)) [I "b"] [Atrib (V "TSsq") (((Var (V "TSsq")) :+: (Const 1)) :%: (Const 128)), Atrib (V "S_credit") ((Var (V "S_credit")) :-: (Const 1))]
tran8 = Transition "t8" (S "s4") (S "s4") (Dif (Var (V "R_credit")) (Const 0) :&: Eq (Var (V "Send_sq")) (Var (V "TRsq"))) [I "b"] [Atrib (V "TRsq") (((Var (V "TRsq")) :+: (Const 1)) :%: (Const 128)), Atrib (V "R_credit") ((Var (V "R_credit")) :-: (Const 1))]

tran9 = Transition "t9" (S "s4") (S "s4") (Eq (Var (V "R_credit")) (Const 0) :|: Dif (Var (V "Send_sq")) (Var (V "TRsq"))) [I "b"] []
tran10 = Transition "t10" (S "s4") (S "s4") (Nil) [I "b"] [Atrib (V "R_credit") ((Var (V "R_credit")) :+: (Var (V "cr")))]
tran11 = Transition "t11" (S "s4") (S "s4") (Gte (Var (V "TSsq")) (Var (V "XpSsq")) :&: Gte ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq"))) (Const 0) :&: Lte ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq"))) (Const 15)) [I "b"] [Atrib (V "S_credit") ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq")))]
tran12 = Transition "t12" (S "s4") (S "s4") (Gte (Var (V "TSsq")) (Var (V "XpSsq")) :&: (Lt ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq"))) (Const 0) :|: Gt ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq"))) (Const 15))) [I "b"] []
tran13 = Transition "t13" (S "s4") (S "s4") (Lt (Var (V "TSsq")) (Var (V "XpSsq")) :&: Gte ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq")) :-: (Const 128)) (Const 0) :&: Lte ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq")) :-: (Const 128)) (Const 15)) [I "b"] [Atrib (V "S_credit") ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq")) :-: (Const 128))]
tran14 = Transition "t14" (S "s4") (S "s4") (Lt (Var (V "TSsq")) (Var (V "XpSsq")) :&: (Lt ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq")) :-: (Const 128)) (Const 0) :|: Gt ((Var (V "cr")) :+: (Var (V "XpSsq")) :-: (Var (V "TSsq")) :-: (Const 128)) (Const 15))) [I "b"] []
tran15 = Transition "t15" (S "s4") (S "s4") (Gt (Var (V "S_Credit")) (Const 0)) [I "b"] []
tran16 = Transition "t16" (S "s4") (S "s5") (Nil) [] []
tran17 = Transition "t17" (S "s4") (S "s6") (Nil) [] []
tran18 = Transition "t18" (S "s6") (S "s1") (Nil) [] []
tran19 = Transition "t19" (S "s5") (S "s1") (Nil) [] []
tran20 = Transition "t20" (S "s5") (S "s1") (Nil) [] []
testPath1 = P ([tran1, tran2, tran3, tran4])

efsm2 :: EFSM
efsm2 =
    EFSM {states = [S "s1", S "s2", S "s3", S "s4", S "s5", S "s6"], transitions = transition2, vars = [V "opt", V "R_credit", V "S_credit", V "TRsq"], start = S "s1"}
