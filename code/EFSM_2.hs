module EFSM_2 where
import Data.List
import Data.Char
import GA

newtype State = S String deriving (Eq, Show)

newtype VarMem = V String deriving (Eq, Show)

newtype Cint = C Int  deriving (Eq, Show)

newtype Input = I String deriving (Eq, Show)

data Condition = Nil |
                Condition :&: Condition |
                Condition :|: Condition |
                T |
                F |
                Lt ExpAr ExpAr |
                Gt ExpAr ExpAr  |
                Lte ExpAr ExpAr |
                Gte ExpAr ExpAr |
                Dif ExpAr ExpAr |
                Eq ExpAr ExpAr deriving (Eq, Show)

data ExpAr = Const Int |
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


-----------------------
--"p1" GT 10 :&: "p1" Lt 20
transition :: [Transition]
transition = [Transition "t1" (S "s1") (S "s1") (Gte (Param "p1") (Const 10) :&: Lte (Param "p1") (Const 20) :&: Gte (Param "p2") (Const 0) :&: Lte (Param "p2") (Const 10)) [I "aa"] [Atrib (V "v1") (Param "p1"), Atrib (V "v2") (Param "p2")], Transition "t2" (S "s1") (S "s2") Nil [I "ab"] [Atrib (V "v3") (Const 10)], Transition "t3" (S "s2") (S "s3") (Gt (Var (V "v3")) (Const 0)) [I "a"] [Atrib (V "v1") (Var (V "v2") :+: Var (V "v3")), Atrib (V "v3") (Var (V "v2")) ], Transition "t4" (S "s3") (S "s1") (Gt (Var (V "v1")) (Var (V "v2"))) [I "ba"] [], Transition "t5" (S "s2") (S "s2") (Lt (Var (V "v3")) (Const 0)) [I "bb"] [Atrib (V "v1") (Const 0)], Transition "t6" (S "s3") (S "s3") (Lt (Var (V "v1")) (Param "p1") :&: Gt (Param "p1") (Param "p2")) [I "b"] []]

efsm :: EFSM
efsm =
    EFSM {states = [S "s1", S "s2", S "s3"], transitions = transition, vars = [V "v1", V "v2", V "v3"], start = S "s1"}
-----------------------

---{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}
newtype Path = P [Transition] deriving (Eq, Show)

isValid1 :: Path -> Bool
isValid1 (P[]) = True
isValid1 (P(p:paths)) = elem p (transitions efsm) && isValid1 (P paths)

isValid2 :: Path -> Bool
isValid2 (P[]) = False
isValid2 (P paths) = s1 t0 == start efsm
                        where t0 = head paths

isValid3 :: Path -> Bool
isValid3 (P paths) = and [s2 t1 == s1 t2 |(t1, t2) <- zip paths (tail paths) ]


isValid :: Path -> Bool
isValid paths = isValid1 paths && isValid2 paths && isValid3 paths
---{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}

-------------------Solution Encoding-------------------
--from Ch to Path

numberOfStates ::  Int
numberOfStates = length (states efsm)

--fct aux pt numberOfTransitions
nOT :: State -> Int
nOT (S st) = length [1 | t <- transition, s1 t == S st]

numberOfTransitions ::  [Int]
numberOfTransitions = [nOT st |  st <- states efsm]

myGcd :: Int -> Int -> Int
myGcd 0 0 = 1
myGcd x 0 = x
myGcd x y = myGcd y (x `mod` y)

myLcm :: Int -> Int -> Int
myLcm x y = (x * y) `div` myGcd x y

myLcmList :: [Int] -> Int
myLcmList [] = 1
myLcmList (x:xs) = if x == 0
                    then myLcmList xs
                else
                    myLcm x (myLcmList xs)

lcmnumberOfTransitions :: Int
lcmnumberOfTransitions = myLcmList numberOfTransitions

divby0 :: Int -> Int -> Int
divby0 x 0 = 1
divby0 x y = div x y

ranges :: [Int]
ranges = [divby0 lcmnumberOfTransitions ni| ni <- numberOfTransitions]

--chromosome to path
leavingStateS :: State -> [Transition]
leavingStateS (S st) = [t | t <- transition, s1 t == S st]


getmThTransLeavingStateS :: State -> Int -> Transition
getmThTransLeavingStateS (S st) m = leavingStateS (S st) !! m

rangesAndStates :: [(State, Int)]
rangesAndStates  = zip (states efsm) ranges

getRangeForState :: State -> Int
getRangeForState (S st) = head [t2 | (t1, t2) <- rangesAndStates, t1 == S st]


chromosomeToPath :: State -> Chromosome -> Path
chromosomeToPath (S st) [] =  P []
chromosomeToPath (S st) (c:chr) = P (t : x)
                                        where
                                            P x = chromosomeToPath st2 chr
                                            m = c `div` getRangeForState (S st)
                                            t = leavingStateS (S st) !! m
                                            st2 = s2 (getmThTransLeavingStateS (S st) m)

-----------------End Solution Encoding-----------------


