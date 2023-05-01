module Helpers where
import EFSM_2
import Data.Maybe
import Data.List

--Assignment Type and Variable Penalty
type VarPenalty = (Int, Int) 

--Guard Penalty?
type Gp = Int

--Dependency
type Dependency = Bool 

--Guard Type
data GType = G_PV | G_VV | G_VC | G_PC | G_PP

--Operator Type
data OpType = OP_PV | OP_VV | OP_VC | NOP

------------------------------------------------
--Comparator Type - added
data CompType = L_t | L_te | G_t | G_te | Diff | Eqq
---------------------------------------------------

--Transitions Array
data TransArray = TrA { 
                            getVarPenalty :: [VarPenalty],
                            getGp :: Gp,
                            getDependecy :: Dependency
                         } deriving (Show, Eq)


--checks if any params

hasParams (Const i) = False
hasParams (Var vv) = False
hasParams (Param p) = True 
hasParams (e1 :+: e2) = hasParams e1 || hasParams e2
hasParams (e1 :-: e2) = hasParams e1 || hasParams e2
hasParams (e1 :*: e2) = hasParams e1 || hasParams e2
hasParams (e1 :/: e2) = hasParams e1 || hasParams e2
hasParams (e1 :%: e2) = hasParams e1 || hasParams e2


--checks if any variables

hasVars (Const i) = False
hasVars (Var vv) = True
hasVars (Param p) = False 
hasVars (e1 :+: e2) = hasVars e1 || hasVars e2
hasVars (e1 :-: e2) = hasVars e1 || hasVars e2
hasVars (e1 :*: e2) = hasVars e1 || hasVars e2
hasVars (e1 :/: e2) = hasVars e1 || hasVars e2
hasVars (e1 :%: e2) = hasVars e1 || hasVars e2

--checks if any const
hasConst e = not (hasParams e || hasVars e)

--get const from expAr
-- getConst :: Condition -> Int
-- getConst (Const i) = i 
-- getConst (Lt e1 e2) = getConstAux e1 e2
-- getConst (Gt e1 e2) = getConstAux e1 e2
-- getConst (Eq e1 e2) = getConstAux e1 e2
-- getConst (Dif e1 e2) = getConstAux e1 e2



--guard type
getGTypeAux e1 e2 = if (hasParams e1 && hasVars e2) || (hasParams e2 && hasVars e1)
                        then G_PV
                      else if (hasParams e1 && hasParams e2) && not (hasVars e1 || hasVars e2)
                        then G_PP
                      else if (hasParams e1 && hasConst e2) || (hasParams e2 && hasConst e1)
                        then G_PC
                      else if (hasVars e1 && hasVars e2) && not (hasParams e1 || hasParams e2)
                        then G_VV
                      else G_VC

evalExpBool :: Condition -> Int -> Bool
evalExpBool (Lt exp1 exp2) i = (evalExpAr exp1 i) < (evalExpAr exp2 i)
evalExpBool (Gt exp1 exp2) i = (evalExpAr exp1 i) > (evalExpAr exp2 i)
evalExpBool (Lte exp1 exp2) i = (evalExpAr exp1 i) <= (evalExpAr exp2 i)
evalExpBool (Gte exp1 exp2) i= (evalExpAr exp1 i) >= (evalExpAr exp2 i)
evalExpBool (Dif exp1 exp2) i = (evalExpAr exp1 i) /= (evalExpAr exp2 i)
evalExpBool (Eq exp1 exp2) i = (evalExpAr exp1 i) == (evalExpAr exp2 i)
evalExpBool _ i = error "Something went wrong!"

evalExpAr :: ExpAr -> Int -> Int
evalExpAr (Const x) i = x
evalExpAr (Var x) i = i
evalExpAr (Param x) i = error "Something went wrong!"
evalExpAr (exp1 :+: exp2) i = (evalExpAr exp1 i) + (evalExpAr exp2 i)
evalExpAr (exp1 :-: exp2) i = (evalExpAr exp1 i) - (evalExpAr exp2 i)
evalExpAr (exp1 :*: exp2) i = (evalExpAr exp1 i) * (evalExpAr exp2 i)
evalExpAr (exp1 :/: exp2) i = (evalExpAr exp1 i) `div` (evalExpAr exp2 i)
evalExpAr (exp1 :%: exp2 ) i = (evalExpAr exp1 i) `mod` (evalExpAr exp2 i)

--0 daca nu exista
getVarIndexFromAssignment :: VarMem -> Int 
getVarIndexFromAssignment var = 1 + (fromMaybe (-1) (elemIndex var (vars efsm)))