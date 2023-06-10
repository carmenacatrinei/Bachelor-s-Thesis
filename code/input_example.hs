module Input_Example 
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