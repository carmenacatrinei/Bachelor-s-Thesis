module FTP where
import Helpers
import EFSM_2 

--get Operator Type
getOpType :: ExpAr -> OpType
getOpType e = if hasParams e == True
                        then OP_PV
                      else if hasVars e == True
                        then OP_VV
                      else OP_VC

--get Guard Type
getGType :: Condition -> GType
getGType (Lt e1 e2) = getGTypeAux e1 e2 
getGType (Lte e1 e2) = getGTypeAux e1 e2 
getGType (Gt e1 e2) = getGTypeAux e1 e2 
getGType (Gte e1 e2) = getGTypeAux e1 e2 
getGType (Dif e1 e2) = getGTypeAux e1 e2 
getGType (Eq e1 e2) = getGTypeAux e1 e2 

----------------------------------------
--get Comp Type - added
getCompType :: Condition -> CompType
getCompType (Lt e1 e2) = L_t
getCompType (Lte e1 e2) = L_te
getCompType (Gt e1 e2) = G_t
getCompType (Gte e1 e2) = G_te
getCompType (Dif e1 e2) = Diff
getCompType (Eq e1 e2) = Eqq 

---------------------------------------

--Penalties values (-2, -1, v1..m or 0)
penaltiesValues :: Condition -> (Gp, Dependency)
penaltiesValues Nil = (0, False)
penaltiesValues T = (0, False)
penaltiesValues F = (0, False)
penaltiesValues (c1 :&: c2) = (gp1+gp2, dep1 || dep2)
    where (gp1,dep1) = localPenalty c1 
          (gp2,dep2) = localPenalty c2 
penaltiesValues (c1 :|: c2) = (min gp1 gp2, dep1 || dep2)
    where (gp1,dep1) = localPenalty c1 
          (gp2,dep2) = localPenalty c2 
penaltiesValues c = localPenalty c


--localpenalty de implementat?
----------------------
--localPenalty nu e de fapt getPenalty? doar ca trebuie un singur param, pt opposed trb functie separata?
----------------------

--FTP.java de uitat + Helper.java + lucrare
--poate reusesti getpenalty din Helper.java 112


-------------------------------------------------

getTransitionArray :: Transition -> Transition -> TransArray 
getTransitionArray tr1 tr2 = TrA penalties gp dep 
    where 
        g1 = condition tr1
        g2 = condition tr2
        dep = False 
        gp = 0
        assignments1 = operations tr1
        variables = vars efsm

        --V var scot constructorul
        penalties = [ computePenalty var  | var <- variables ]

        computePenalty cv =  (penalty , varP)
            where 
                penalty = 0
                l = [exp | Atrib v exp <- assignments1, v == cv]
                varP = if length l == 0 
                        then 0
                        else let expr = head l 
                                 op = getOpType expr in
                                    case op of 
                                        OP_VC -> -2
                                        OP_PV -> -1
                                        OP_VV -> getVarIndexFromAssignment cv  

                
----------------------------------------------------------------------------------------------------

--conditia din guard t2
--e atrib t1

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
                              else
                                if getCompType cond == Eqq 
                                    then  4
                                  else if getCompType cond == L_t || getCompType cond == G_t 
                                    then  3
                                  else if getCompType cond == L_te || getCompType cond == G_te
                                    then  2
                                  else  1
                          ---------G_VV
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
                                    then   24
                                  else   16
                              else if getOpType e == OP_VC 
                                then
                                  if getCompType cond == Eqq 
                                    then   60
                                  else if getCompType cond == L_t || getCompType cond == G_t 
                                    then   48
                                  else if getCompType cond == L_te || getCompType cond == G_te
                                    then   36
                                  else   24
                              else
                                if getCompType cond == Eqq 
                                    then   16
                                  else if getCompType cond == L_t || getCompType cond == G_t 
                                    then   12
                                  else if getCompType cond == L_te || getCompType cond == G_te
                                    then   8
                                  else   4
                          ---------G_VC
                          else if getGType cond == G_VC
                            then
                              if getOpType e == OP_PV 
                                then 
                                  if getCompType cond == Eqq 
                                    then   30
                                  else if getCompType cond == L_t || getCompType cond == G_t 
                                    then   24
                                  else if getCompType cond == L_te || getCompType cond == G_te
                                    then   18
                                  else   12
                              else if getOpType e == OP_VV 
                                then
                                  if getCompType cond == Eqq 
                                    then   60
                                  else if getCompType cond == L_t || getCompType cond == G_t 
                                    then   48
                                  else if getCompType cond == L_te || getCompType cond == G_te
                                    then   36
                                  else   24
                              else if getOpType e == OP_VC 
                                then
                                  --INF if False and 0 otherwise
                                  let Const i = e in
                                    if evalExpBool cond i == False
                                      then   10000
                                    else   0
                                  ---------
                              else
                                if getCompType cond == Eqq 
                                    then   40
                                  else if getCompType cond == L_t || getCompType cond == G_t 
                                    then   32
                                  else if getCompType cond == L_te || getCompType cond == G_te
                                    then   24
                                  else   16
                          ---------G_PC
                          else if getGType cond == G_PC
                            then --se subintelege ca e NOP?
                              if getOpType e == OP_VC || getOpType e == OP_VV || getOpType e == OP_PV
                                then   0
                              else
                                if getCompType cond == Eqq 
                                  then   12
                                else if getCompType cond == L_t || getCompType cond == G_t 
                                  then   8
                                else if getCompType cond == L_te || getCompType cond == G_te
                                  then   4
                                else   1
                          -----------G_PP
                          --else if getGType cond == G_PP
                          else
                              if getOpType e == OP_VC || getOpType e == OP_VV || getOpType e == OP_PV
                                then   0
                              else
                                if getCompType cond == Eqq 
                                  then   6
                                else if getCompType cond == L_t || getCompType cond == G_t 
                                  then   4
                                else if getCompType cond == L_te || getCompType cond == G_te
                                  then   2
                                else  1
                         
---------------------------------------------------------------        
--prima atribuire din ti care contine var curenta
--pt fiecare var 

---------------------------------------------------------------




--Localpenalty?       
--getVarIndexFromAssignment
--opposed cum ar trb calculat?  
--assignment is opposed to a guard
--op_vc de g_Vc
--const  care apar in op_vc si g_vc sunt aceleasi si gop e < > dif
--const care apar in op_vc si g_vc sunt diferite si gop e =

--ex
--Lt (Var (V "v3")) (Const 0) / Atrib (V "v1") (Const 0)
-- opposed :: Condition -> -> Bool 
-- opposed exp = opp 
-- where
--   opp = False
--     if getGType exp == G_VC and getOpType exp == OP_VC 
--       then
--         if getCompType exp == Eqq 
--           then 
--             if 
--             --daca const sunt dif => opposed = True
--         else if getCompType exp == L_t or getCompType exp == G_t or getCompType exp == Diff 
--           then
--             --daca const sunt egale => opposed = False

--             --trebuie scoasa const de aici


--getVarIndexFromAssignment :: Operations -> Int 
--op de forma Atrib (V "vx") (....)
--cum iau doar V ul din tot assignment ul?
--getVarIndexFromAssignment assignment = tail (Atrib assignment)
