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


