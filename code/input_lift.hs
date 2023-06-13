module Input_Lift 
where 
import Data.List 
import Data.Char 
import EFSM_2

--Lift System EFSM
transition :: [Transition]
transition = [tran1, tran2, tran3, tran4, tran5, tran6, tran7, tran8, tran9, tran10, tran11, tran12, tran13, tran14, tran15, tran16, tran17, tran18, tran19, tran20]

--pentru tran0 cum ar trebui modificat?
--tran0 = Transition "t0" (S "s1") (S "s2") (Nil) [I "prop_out"] [Atrib (V "opt") (Param "prop_out"), Atrib (V "R_credit") (Const 0)]
tran1 = Transition "t1" (S "s0") (S "s0") (Gte (Param "Pos") (Const 0) :&: Lte (Param "Pos") (Const 15) :&: Eq (Var (V "DrSt")) (Const 0)) [I "Pos"] [Atrib (V "DrSt") (Const 1)]
tran2 = Transition "t2" (S "s0") (S "s0") (Gte (Param "Pos") (Const 0) :&: Lte (Param "Pos") (Const 15) :&: Eq (Var (V "DrSt")) (Const 1)) [I "Pos", I "Pw"] [Atrib (V "DrSt") (Const 0)]
tran3 = Transition "t3" (S "s0") (S "s1") (Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25) :&: Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 1)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 1)]
tran4 = Transition "t4" (S "s1") (S "s0") (Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25) :&: Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 0)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 0)]
tran5 = Transition "t5" (S "s0") (S "s1") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 1) :&: Eq (Var (V "w")) (Const 0) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 1)]
tran6 = Transition "t6" (S "s1") (S "s0") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 0) :&: Eq (Var (V "w")) (Const 0) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 0)]
tran7 = Transition "t7" (S "s1") (S "s1") (Gte (Param "Pos") (Const 0) :&: Lte (Param "Pos") (Const 15) :&: Eq (Var (V "DrSt")) (Const 0)) [I "Pos"] [Atrib (V "DrSt") (Const 1)]
tran8 = Transition "t8" (S "s1") (S "s1") (Gte (Param "Pos") (Const 0) :&: Lte (Param "Pos") (Const 15) :&: Eq (Var (V "DrSt")) (Const 1)) [I "Pos", I "Pw"] [Atrib (V "DrSt") (Const 0), Atrib (V "w") (Param "Pw")]
tran9 = Transition "t9" (S "s1") (S "s2") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 2) :&: Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 2)]
tran10 = Transition "t10" (S "s2") (S "s1") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 1) :&: Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 1)]
tran11 = Transition "t11" (S "s2") (S "s1") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 1) :&: Eq (Var (V "w")) (Const 0) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 1)]
tran12 = Transition "t12" (S "s1") (S "s2") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 2) :&: Eq (Var (V "w")) (Const 0) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 2)]
tran13 = Transition "t13" (S "s2") (S "s2") (Gte (Param "Pos") (Const 0) :&: Lte (Param "Pos") (Const 15) :&: Eq (Var (V "DrSt")) (Const 0)) [I "Pos"] [Atrib (V "DrSt") (Const 1)]
tran14 = Transition "t14" (S "s2") (S "s2") (Gte (Param "Pos") (Const 0) :&: Lte (Param "Pos") (Const 15) :&: Eq (Var (V "DrSt")) (Const 1)) [I "Pos", I "Pw"] [Atrib (V "DrSt") (Const 0), Atrib (V "w") (Param "Pw")]
tran15 = Transition "t15" (S "s2") (S "s0") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 0) :&: Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 0)]
tran16 = Transition "t16" (S "s0") (S "s2") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 2) :&: Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 2)]
tran17 = Transition "t17" (S "s0") (S "s2") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 2) :&: Eq (Var (V "w")) (Const 0) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 2)]
tran18 = Transition "t18" (S "s2") (S "s0") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 0) :&: Eq (Var (V "w")) (Const 0) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 0)]
tran19 = Transition "t19" (S "s0") (S "ss") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 100) :&: Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 100)]
tran20 = Transition "t20" (S "ss") (S "s0") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 0) ) [I "Pf"] [Atrib (V "Floor") (Const 0)]
tran21 = Transition "t21" (S "ss") (S "s1") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 1) ) [I "Pf"] [Atrib (V "Floor") (Const 1)]
tran22 = Transition "t22" (S "s1") (S "ss") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 100) :&: Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 100)]
tran23 = Transition "t23" (S "s2") (S "ss") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 100) :&: Gte (Var (V "w")) (Const 15) :&: Lte (Var (V "w")) (Const 250) :&: Gte (Param "Ph") (Const 10) :&: Lte (Param "Ph") (Const 35) :&: Gte (Param "Ps") (Const 0) :&: Lte (Param "Ps") (Const 25)) [I "Pf", I "Ph", I "Ps"] [Atrib (V "Floor") (Const 100)]
tran24 = Transition "t24" (S "ss") (S "s2") (Eq (Var (V "DrSt")) (Const 0) :&: Eq (Param "Pf") (Const 2) ) [I "Pf"] [Atrib (V "Floor") (Const 2)]

testPath = P ([tran1, tran2, tran3, tran4])

efsm :: EFSM
efsm =
    EFSM {states = [S "s0", S "s1", S "s2", S "ss"], transitions = transition, vars = [V "DrSt", V "w", V "Floor"], start = S "s0"}

