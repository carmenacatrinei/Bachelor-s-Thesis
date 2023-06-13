module EFSM_2 where
import Data.List
import Data.Char

newtype State = S String deriving (Eq, Show)

newtype VarMem = V String deriving (Eq, Show)

newtype Cint = C Int  deriving (Eq, Show)

newtype Input = I String deriving (Eq, Show)

data Condition = Nil |
                Condition :&: Condition |
                Condition :|: Condition |
                T |
                F |
                Eq ExpAr ExpAr |
                Lt ExpAr ExpAr |
                Gt ExpAr ExpAr  |
                Lte ExpAr ExpAr |
                Gte ExpAr ExpAr |
                Dif ExpAr ExpAr 
                 deriving (Eq, Show)

data ExpAr = NilE |
             Const Int | 
             Var VarMem |
             Param String |
             ExpAr :+: ExpAr |
             ExpAr :-: ExpAr |
             ExpAr :*: ExpAr |
             ExpAr :/: ExpAr |
             ExpAr :%: ExpAr deriving (Eq, Show)

data Operations = Atrib VarMem ExpAr deriving (Eq, Show) 

--field labels
data Transition =
    Transition {
                    name :: String,
                    s1 :: State,
                    s2 :: State,
                    condition :: Condition,
                    input :: [Input],
                    operations :: [Operations]
                } deriving (Eq, Show)

data EFSM =
    EFSM {
            states :: [State],
            transitions :: [Transition],
            vars :: [VarMem],
            start :: State
        } deriving (Eq, Show)

newtype Path = P [Transition] deriving (Eq, Show)

-----------------------
--"p1" GT 10 :&: "p1" Lt 20
-- transition :: [Transition]
-- --transition = [Transition "t1" (S "s1") (S "s1") (Gte (Param "p1") (Const 10) :&: Lte (Param "p1") (Const 20) :&: Gte (Param "p2") (Const 0) :&: Lte (Param "p2") (Const 10)) [I "aa"] [Atrib (V "v1") (Param "p1"), Atrib (V "v2") (Param "p2")], Transition "t2" (S "s1") (S "s2") Nil [I "ab"] [Atrib (V "v3") (Const 10)], Transition "t3" (S "s2") (S "s3") (Gt (Var (V "v3")) (Const 0)) [I "a"] [Atrib (V "v1") (Var (V "v2") :+: Var (V "v3")), Atrib (V "v3") (Var (V "v2")) ], Transition "t4" (S "s3") (S "s1") (Gt (Var (V "v1")) (Var (V "v2"))) [I "ba"] [], Transition "t5" (S "s2") (S "s2") (Lt (Var (V "v3")) (Const 0)) [I "bb"] [Atrib (V "v1") (Const 0)], Transition "t6" (S "s3") (S "s3") (Lt (Var (V "v1")) (Param "p1") :&: Gt (Param "p1") (Param "p2")) [I "b"] []]
-- transition = [tran1, tran2, tran3, tran4, tran5, tran6]

-- tran1 = Transition "t1" (S "s1") (S "s1") (Gte (Param "p1") (Const 10) :&: Lte (Param "p1") (Const 20) :&: Gte (Param "p2") (Const 0) :&: Lte (Param "p2") (Const 10)) [I "aa"] [Atrib (V "v1") (Param "p1"), Atrib (V "v2") (Param "p2")]
-- tran2 = Transition "t2" (S "s1") (S "s2") Nil [I "ab"] [Atrib (V "v3") (Const 10)]
-- tran3 = Transition "t3" (S "s2") (S "s3") (Gt (Var (V "v3")) (Const 0)) [I "a"] [Atrib (V "v1") (Var (V "v2") :+: Var (V "v3")), Atrib (V "v3") (Var (V "v2")) ]
-- tran4 = Transition "t4" (S "s3") (S "s1") (Gt (Var (V "v1")) (Var (V "v2"))) [I "ba"] []
-- tran5 = Transition "t5" (S "s2") (S "s2") (Lt (Var (V "v3")) (Const 0)) [I "bb"] [Atrib (V "v1") (Const 0)]
-- tran6 = Transition "t6" (S "s3") (S "s3") (Lt (Var (V "v1")) (Param "p1") :&: Gt (Param "p1") (Param "p2")) [I "b"] []

-- testPath = P ([tran1, tran2, tran3, tran4])

-- efsm :: EFSM
-- efsm =
--     EFSM {states = [S "s1", S "s2", S "s3"], transitions = transition, vars = [V "v1", V "v2", V "v3"], start = S "s1"}
-- -----------------------




