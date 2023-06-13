module Input_Transport
where 
import Data.List 
import Data.Char 
import EFSM_2
--Class2 Transfer Protocol EFSM
--tran8, tran9,
transition :: [Transition]
transition = [tran0, tran1, tran2, tran3, tran4, tran5, tran6, tran7, tran8, tran9, tran10, tran11, tran12, tran13, tran14, tran15, tran16, tran17, tran18, tran19, tran20]

tran0 = Transition "t0" (S "s1") (S "s2") (Nil) [I "prop_out"] [Atrib (V "opt") (Param "prop_out"), Atrib (V "R_credit") (Const 0)]
tran1 = Transition "t1" (S "s1") (S "s3") (Nil) [I "opt_ind", I "cr"] [Atrib (V "opt") (Param"opt_ind"), Atrib (V "S_credit") (Param "cr"), Atrib (V "R_credit") (Const 0)]
tran2 = Transition "t2" (S "s2") (S "s4") (Lt (Param "opt_ind") (Var (V "opt"))) [I "opt_ind", I "cr"] [Atrib (V "TRsq") (Const 0), Atrib (V "TSsq") (Const 0), Atrib (V "opt") (Param "opt_ind"), Atrib (V "S_credit") (Param "cr")]
tran3 = Transition "t3" (S "s2") (S "s5") (Gt (Param "opt_ind") (Var (V "opt"))) [I "opt_ind", I "cr"] []
tran4 = Transition "t4" (S "s2") (S "s1") (Nil) [] []
tran5 = Transition "t5" (S "s3") (S "s4") (Lt (Param "accpt_opt") (Var (V "opt"))) [I "accpt_opt"] [Atrib (V "opt") (Param "accpt_opt"), Atrib (V "TRsq") (Const 0), Atrib (V "TSsq") (Const 0)]
tran6 = Transition "t6" (S "s3") (S "s6") (Nil) [] []
tran7 = Transition "t7" (S "s4") (S "s4") (Gt (Var (V "S_credit")) (Const 0)) [I "Udata"] [Atrib (V "TSsq") (((Var (V "TSsq")) :+: (Const 1)) :%: (Const 128)), Atrib (V "S_credit") ((Var (V "S_credit")) :-: (Const 1))]
tran8 = Transition "t8" (S "s4") (S "s4") (Eq (Param "Send_sq") (Var (V "TRsq")) :&: Dif (Var (V "R_credit")) (Const 0)) [I "Send_sq"] [Atrib (V "TRsq") (((Var (V "TRsq")) :+: (Const 1)) :%: (Const 128)), Atrib (V "R_credit") ((Var (V "R_credit")) :-: (Const 1))]
tran9 = Transition "t9" (S "s4") (S "s4") (Eq (Var (V "R_credit")) (Const 0) :|: (Dif (Param "Send_sq") (Var (V "TRsq")))) [I "Send_sq"] []
tran10 = Transition "t10" (S "s4") (S "s4") (Nil) [I "cr"] [Atrib (V "R_credit") ((Var (V "R_credit")) :+: (Param "cr"))]
tran11 = Transition "t11" (S "s4") (S "s4") (Gte (Var (V "TSsq")) (Param "XpSsq") :&: (Gte ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq"))) (Const 0) :&: (Lte ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq"))) (Const 15)))) [I "XpSsq", I "cr"] [Atrib (V "S_credit") ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq")))]
tran12 = Transition "t12" (S "s4") (S "s4") (Gte (Var (V "TSsq")) (Param "XpSsq") :&: (Lt ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq"))) (Const 0) :|: (Gt ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq"))) (Const 15)))) [I "XpSsq", I "cr"] []
tran13 = Transition "t13" (S "s4") (S "s4") (Lt (Var (V "TSsq")) (Param "XpSsq") :&: (Gte ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq")) :-: (Const 128)) (Const 0) :&: (Lte ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq")) :-: (Const 128)) (Const 15)))) [I "XpSsq", I "cr"] [Atrib (V "S_credit") ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq")) :-: (Const 128))]
tran14 = Transition "t14" (S "s4") (S "s4") (Lt (Var (V "TSsq")) (Param "XpSsq") :&: (Lt ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq")) :-: (Const 128)) (Const 0) :|: (Gt ((Param "cr") :+: (Param "XpSsq") :-: (Var (V "TSsq")) :-: (Const 128)) (Const 15)))) [I "XpSsq", I "cr"] []
tran15 = Transition "t15" (S "s4") (S "s4") (Gt (Var (V "S_Credit")) (Const 0)) [] []
tran16 = Transition "t16" (S "s4") (S "s5") (Nil) [] []
tran17 = Transition "t17" (S "s4") (S "s6") (Nil) [] []
tran18 = Transition "t18" (S "s6") (S "s1") (Nil) [] []
tran19 = Transition "t19" (S "s5") (S "s1") (Nil) [] []
tran20 = Transition "t20" (S "s5") (S "s1") (Nil) [] []
testPath = P ([tran1, tran2, tran3, tran4])

efsm :: EFSM
efsm =
    EFSM {states = [S "s1", S "s2", S "s3", S "s4", S "s5", S "s6"], transitions = transition, vars = [V "opt", V "R_credit", V "S_credit", V "TRsq", V "TSsq"], start = S "s1"}

