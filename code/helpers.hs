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
data GType = G_PV | G_VV | G_VC | G_PC | G_PP deriving (Eq, Show)

--Operator Type
data OpType = OP_PV | OP_VV | OP_VC | NOP deriving (Eq, Show)

------------------------------------------------
--Comparator Type - added
data CompType = L_t | L_te | G_t | G_te | Diff | Eqq deriving (Eq, Show)
---------------------------------------------------

--Transitions Array
data TransArray = TrA { 
                            getVarPenalty :: [VarPenalty],
                            getGp :: Gp,
                            getDependecy :: Dependency
                         } deriving (Show, Eq)

-------------------------------
--getConst scoate const dintr-o expresie aritmetica si intoarce Int
getConst :: ExpAr -> Int 
getConst (NilE) = 0
getConst (Const i) = i
getConst (Var vv) = 0
getConst (Param p) = 0 
getConst (e1 :+: e2) = getConst e1 + getConst e2
getConst (e1 :-: e2) = getConst e1 - getConst e2
getConst (e1 :*: e2) = getConst e1 * getConst e2
getConst (e1 :/: e2) = getConst e1 `div` getConst e2
getConst (e1 :%: e2) = getConst e1 `mod` getConst e2
--------------------------------------------

--getVar scoate var dintr-o expresie aritmetica si intoarce VarMem
getVar :: ExpAr -> VarMem
getVar (NilE) = error "Something went wrong!"
getVar (Const i) = error "Something went wrong!"
getVar (Var vv) = vv
getVar (Param p) = error "Something went wrong!"
getVar (e1 :+: e2) = getVar e1 
getVar (e1 :-: e2) = getVar e1 
getVar (e1 :*: e2) = getVar e1 
getVar (e1 :/: e2) = getVar e1 
getVar (e1 :%: e2) = getVar e1 
--------------------
--checks if any params
hasParams :: ExpAr -> Bool
hasParams (Const i) = False
hasParams (Var vv) = False
hasParams (Param p) = True 
hasParams (e1 :+: e2) = hasParams e1 || hasParams e2
hasParams (e1 :-: e2) = hasParams e1 || hasParams e2
hasParams (e1 :*: e2) = hasParams e1 || hasParams e2
hasParams (e1 :/: e2) = hasParams e1 || hasParams e2
hasParams (e1 :%: e2) = hasParams e1 || hasParams e2
hasParams _ = False

--checks if any variables
hasVars :: ExpAr -> Bool
hasVars (Const i) = False
hasVars (Var vv) = True
hasVars (Param p) = False 
hasVars (e1 :+: e2) = hasVars e1 || hasVars e2
hasVars (e1 :-: e2) = hasVars e1 || hasVars e2
hasVars (e1 :*: e2) = hasVars e1 || hasVars e2
hasVars (e1 :/: e2) = hasVars e1 || hasVars e2
hasVars (e1 :%: e2) = hasVars e1 || hasVars e2
hasVars _ = False

--checks if any const
hasConst :: ExpAr -> Bool
hasConst NilE = False
hasConst e = not (hasParams e || hasVars e)

--contine var curenta
--primeste o expresie aritmetica si o variabila de memorie si verifica daca expresia contine variabila data ca param
--intoarce bool
hasCv :: ExpAr -> VarMem -> Bool
hasCv (NilE) cv = False
hasCv (Const i) cv = False
hasCv (Var (V vv)) (V cv) = vv == cv
hasCv (Param p) cv = False 
hasCv (e1 :+: e2) cv = hasCv e1 cv || hasCv e2 cv
hasCv (e1 :-: e2) cv = hasCv e1 cv || hasCv e2 cv
hasCv (e1 :*: e2) cv = hasCv e1 cv || hasCv e2 cv
hasCv (e1 :/: e2) cv = hasCv e1 cv || hasCv e2 cv
hasCv (e1 :%: e2) cv = hasCv e1 cv || hasCv e2 cv

---------------------------------------------------------------

--c1 and c2
--primeste o conditie si intoarce expresia din stanga
getLeftExp :: Condition -> ExpAr
getLeftExp (Lt e1 e2) = e1
getLeftExp (Gt e1 e2) = e1
getLeftExp (Lte e1 e2) = e1
getLeftExp (Gte e1 e2) = e1
getLeftExp (Dif e1 e2) = e1
getLeftExp (Eq e1 e2) = e1
getLeftExp (Nil) = NilE

--cazurile T F NU SUNT ACOPERITE, functie PENTRU CONDITIA DIN AFFECTING unde :&: si :|: nu sunt acoperite
getLeftExp2 :: Condition -> ExpAr
getLeftExp2 (Nil) = NilE
getLeftExp2 (c1 :&: c2) = getLeftExp2 c1
getLeftExp2 (c1 :|: c2) = getLeftExp2 c1
getLeftExp2 (Lt e1 e2) = e1
getLeftExp2 (Gt e1 e2) = e1
getLeftExp2 (Lte e1 e2) = e1
getLeftExp2 (Gte e1 e2) = e1
getLeftExp2 (Dif e1 e2) = e1
getLeftExp2 (Eq e1 e2) = e1

--getLeftExp (_ c1 c2) = c1   NU _ IN LOC DE CONSTRUCTOR 

--primeste o conditie si intoarce expresia din dreapta
getRightExp :: Condition -> ExpAr
getRightExp (Lt e1 e2) = e2
getRightExp (Gt e1 e2) = e2
getRightExp (Lte e1 e2) = e2
getRightExp (Gte e1 e2) = e2
getRightExp (Dif e1 e2) = e2
getRightExp (Eq e1 e2) = e2
getRightExp (Nil) = NilE

--cazurile T F NU SUNT ACOPERITE, PENTRU CONDITIA DIN AFFECTING
getRightExp2 :: Condition -> ExpAr
getRightExp2 (c1 :&: c2) = getLeftExp2 c1
getRightExp2 (c1 :|: c2) = getLeftExp2 c1
getRightExp2 (Lt e1 e2) = e2
getRightExp2 (Gt e1 e2) = e2
getRightExp2 (Lte e1 e2) = e2
getRightExp2 (Gte e1 e2) = e2
getRightExp2 (Dif e1 e2) = e2
getRightExp2 (Eq e1 e2) = e2
getRightExp2 (Nil) = NilE

-----------------------------------------------------

--guard type Aux
getGTypeAux :: ExpAr -> ExpAr -> GType
getGTypeAux e1 e2 = if (hasParams e1 && hasVars e2) || (hasParams e2 && hasVars e1) && not(hasConst e1 || hasConst e2)
                        then G_PV
                        else if (hasParams e1 && hasParams e2) && not (hasVars e1 || hasVars e2) && not(hasConst e1 || hasConst e2)
                          then G_PP
                        else if (hasParams e1 && hasConst e2) || (hasParams e2 && hasConst e1)
                          then G_PC
                        else if (hasVars e1 && hasVars e2) && not (hasParams e1 || hasParams e2) && not(hasConst e1 || hasConst e2)
                          then G_VV
                        else G_VC

--get Guard Type
getGType :: Condition -> GType
getGType (Lt e1 e2) = getGTypeAux e1 e2 
getGType (Lte e1 e2) = getGTypeAux e1 e2 
getGType (Gt e1 e2) = getGTypeAux e1 e2 
getGType (Gte e1 e2) = getGTypeAux e1 e2 
getGType (Dif e1 e2) = getGTypeAux e1 e2 
getGType (Eq e1 e2) = getGTypeAux e1 e2 
getGType _ = error "Something went wrong!"

-----------------------------------------------------------

--get Operator Type
getOpType :: ExpAr -> OpType
getOpType NilE = NOP
getOpType e = if hasParams e == True
                        then OP_PV
                        else if hasVars e == True && hasConst e == False
                          then OP_VV
                        else OP_VC

-----------------------------------------------------------

--get Comp Type - added
getCompType :: Condition -> CompType
getCompType (Lt e1 e2) = L_t
getCompType (Lte e1 e2) = L_te
getCompType (Gt e1 e2) = G_t
getCompType (Gte e1 e2) = G_te
getCompType (Dif e1 e2) = Diff
getCompType (Eq e1 e2) = Eqq 

------------------------------------------------------------

evalExpBool :: Condition -> Int -> Bool
evalExpBool (Lt exp1 exp2) i = (evalExpAr exp1 i) < (evalExpAr exp2 i)
evalExpBool (Gt exp1 exp2) i = (evalExpAr exp1 i) > (evalExpAr exp2 i)
evalExpBool (Lte exp1 exp2) i = (evalExpAr exp1 i) <= (evalExpAr exp2 i)
evalExpBool (Gte exp1 exp2) i= (evalExpAr exp1 i) >= (evalExpAr exp2 i)
evalExpBool (Dif exp1 exp2) i = (evalExpAr exp1 i) /= (evalExpAr exp2 i)
evalExpBool (Eq exp1 exp2) i = (evalExpAr exp1 i) == (evalExpAr exp2 i)
evalExpBool _ i = error "Something went wrong!"

evalExpAr :: ExpAr -> Int -> Int
evalExpAr (NilE) i = error "Something went wrong!"
evalExpAr (Const x) i = x
evalExpAr (Var x) i = i
evalExpAr (Param x) i = error "Something went wrong!"
evalExpAr (exp1 :+: exp2) i = (evalExpAr exp1 i) + (evalExpAr exp2 i)
evalExpAr (exp1 :-: exp2) i = (evalExpAr exp1 i) - (evalExpAr exp2 i)
evalExpAr (exp1 :*: exp2) i = (evalExpAr exp1 i) * (evalExpAr exp2 i)
evalExpAr (exp1 :/: exp2) i = (evalExpAr exp1 i) `div` (evalExpAr exp2 i)
evalExpAr (exp1 :%: exp2 ) i = (evalExpAr exp1 i) `mod` (evalExpAr exp2 i)

--------------------------------------------------------------

--0 daca nu exista
--primeste o variabila de memorie si intoarce indexul acesteia
getVarIndexFromAssignment :: VarMem -> Int 
getVarIndexFromAssignment var = 1 + (fromMaybe (-1) (elemIndex var (vars efsm)))

--primeste o expresie aritmetica dintr-o operatie de tip VV si intoarce indexul primei var gasite
--adica din v1 = v2 intoarce 2, din v1 = v2 + v3 intoarce 2
getVarIndexFromAssignment2 :: ExpAr -> Int
getVarIndexFromAssignment2 exp = if getOpType exp == OP_VV
                                  then 1 + (fromMaybe (-1) (elemIndex (getVar exp) (vars efsm)))
                                  else 0

--getVarFromOperation (Assignment)
getVarFromOperation :: Operations -> VarMem
getVarFromOperation (Atrib v exp) = v

--getExpFromOperation (Assignment)
getExpFromOperation :: Operations -> ExpAr
getExpFromOperation (Atrib v exp) = exp

---------------------------------------------------------------
--normalizare
norm :: Double -> Double
norm d = if d > 0
          then 1 - 1.001 ** (-d)
          else 0

-------------------------------------------------------------------------
--primeste o conditie si verifica daca aceasta contine < > sau !=
gop :: Condition -> Bool 
gop cond1 =  getCompType cond1 == L_t || getCompType cond1 == G_t || getCompType cond1 == Diff

--primeste o conditie si verifica daca aceasta contine =
gopEq :: Condition->Bool
gopEq cond2 = getCompType cond2 == Eqq 

-- <, >, != && =
gop1 :: Condition -> Condition -> Bool 
gop1 cond1 cond2 = (getCompType cond1 == L_t || getCompType cond1 == G_t || getCompType cond1 == Diff) && getCompType cond2 == Eqq 

-- < <= && >
gop2 :: Condition -> Condition -> Bool 
gop2 cond1 cond2 = (getCompType cond1 == L_t || getCompType cond1 == L_te) && getCompType cond2 == G_t

-- > >= && <
gop3 :: Condition -> Condition -> Bool 
gop3 cond1 cond2 = (getCompType cond1 == G_t || getCompType cond1 == G_te) && getCompType cond2 == L_t

--------------------------
--primeste o conditie si expresia artimetica din assignment si un bool (opposed) si intoarce valoarea de penalty din tabel
getPenalty ::  Condition -> ExpAr -> Bool -> Int
getPenalty cond e opposed = penalty
  where
    penalty =
      if opposed == True 
        then 10000
      else
      ---G_PV
        if getGType cond == G_PV 
          then 
            if getOpType e == OP_PV 
              then 
                if getCompType cond == Eqq 
                  then 8
                  else if getCompType cond == L_t || getCompType cond == G_t 
                    then  6
                  else if getCompType cond == L_te || getCompType cond == G_te
                    then  4
                  else  2
              else if getOpType e == OP_VV 
                then
                  if getCompType cond == Eqq 
                    then 16
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 12
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 8
                    else  4
              else if getOpType e == OP_VC 
                then
                  if getCompType cond == Eqq 
                      then  24
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 18
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 12
                    else  6
              else  --end G_PV
                if getCompType cond == Eqq 
                  then  4
                  else if getCompType cond == L_t || getCompType cond == G_t 
                    then  3
                  else if getCompType cond == L_te || getCompType cond == G_te
                    then  2
                  else  1
      ---G_VV
          else if getGType cond == G_VV 
            then
              if getOpType e == OP_PV 
                then 
                  if getCompType cond == Eqq 
                    then 20
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 16
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 12
                    else 8
                else if getOpType e == OP_VV 
                  then
                    if getCompType cond == Eqq 
                      then 40
                      else if getCompType cond == L_t || getCompType cond == G_t 
                        then 32
                      else if getCompType cond == L_te || getCompType cond == G_te
                        then 24
                      else 16
                else if getOpType e == OP_VC 
                  then
                  if getCompType cond == Eqq 
                    then 60
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 48
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 36
                    else 24
                else  --end G_VV
                  if getCompType cond == Eqq 
                    then 16
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 12
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 8
                    else 4
      ---G_VC
          else if getGType cond == G_VC
            then
              if getOpType e == OP_PV 
                then 
                  if getCompType cond == Eqq 
                    then 30
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 24
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 18
                    else 12
                else if getOpType e == OP_VV 
                  then
                    if getCompType cond == Eqq 
                      then 60
                      else if getCompType cond == L_t || getCompType cond == G_t 
                        then 48
                      else if getCompType cond == L_te || getCompType cond == G_te
                        then 36
                      else 24
                else if getOpType e == OP_VC 
                  then
                    --INF if False and 0 otherwise 
                    -- merge doar pentru o singura variabila
                    let Const i = e in
                      if evalExpBool cond i == False
                        then 10000
                        else 0
                    ---------
                else --end G_VC
                  if getCompType cond == Eqq 
                    then 40
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 32
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 24
                    else 16
      ---G_PC
          else if getGType cond == G_PC
            then
              if getOpType e == OP_VC || getOpType e == OP_VV || getOpType e == OP_PV
                then 0
                else  --end G_PC
                  if getCompType cond == Eqq 
                    then 12
                    else if getCompType cond == L_t || getCompType cond == G_t 
                      then 8
                    else if getCompType cond == L_te || getCompType cond == G_te
                      then 4
                    else 1
      ---G_PP
      --else if getGType cond == G_PP
          else --end getGType
            if getOpType e == OP_VC || getOpType e == OP_VV || getOpType e == OP_PV
              then 0
              else
                if getCompType cond == Eqq 
                  then 6
                  else if getCompType cond == L_t || getCompType cond == G_t 
                    then 4
                  else if getCompType cond == L_te || getCompType cond == G_te
                    then 2
                  else 1
                         
---------------------------------------------------------------  


--isDefClear
-- isDefClear :: Int -> Int -> Int -> Bool 
-- isDefClear iT1 iT2 iT3 = 

--check method

--compute
