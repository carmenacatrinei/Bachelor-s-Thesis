module FTP where
import Helpers
import EFSM_2 
---------------------------------------

--Penalties values for variables
penaltiesValues :: Condition -> Condition -> ExpAr -> VarMem -> (Int, Dependency)
penaltiesValues Nil _ _ _ = (0, False)
penaltiesValues T _ _ _ = (0, False)
penaltiesValues F _ _ _ = (0, False)
penaltiesValues (c1 :&: c2) c3 e v = (vp1+vp2, dep1 || dep2)
    where (vp1,dep1) = penaltiesValues c1 c3 e v
          (vp2,dep2) = penaltiesValues c2 c3 e v
penaltiesValues (c1 :|: c2) c3 e v = (min vp1 vp2, dep1 || dep2)
    where (vp1,dep1) = penaltiesValues c1 c3 e v
          (vp2,dep2) = penaltiesValues c2 c3 e v 
penaltiesValues c c3 e v = localPenalty c c3 e v


--calculul gp-ului
guardPenaltyValue :: Condition -> ExpAr -> Gp
guardPenaltyValue Nil _ = 0
guardPenaltyValue T _ = 0
guardPenaltyValue F _ = 0
guardPenaltyValue (c1 :&: c2) exp = gp1 + gp2 
  where
    gp1 = guardPenaltyValue c1 exp
    gp2 = guardPenaltyValue c2 exp 
guardPenaltyValue (c1 :|: c2) exp = min gp1 gp2 
  where
    gp1 = guardPenaltyValue c1 exp 
    gp2 = guardPenaltyValue c2 exp 
guardPenaltyValue cond exp = gp 
  where
     penaltyLocal = getPenalty cond exp False 
     gt = getGType cond
     gp = if gt == G_PC || gt == G_PP
          then penaltyLocal
          else 0


localPenalty :: Condition -> Condition -> ExpAr -> VarMem -> (Int, Dependency) --Int pentru penalty-ul fiecarei variabile
localPenalty cond cond1 exp cv = (vp, dep)
    where
      gt = getGType cond
      gt1 = getGType cond1
      op = getOpType exp
      c1 = ""
      c2 = ""

      constLeft = if hasCv (getLeftExp cond) cv 
        then  True 
        else False 

      constRight = if hasCv (getRightExp cond) cv 
        then True 
        else False

      constLeft1 = if hasCv (getLeftExp cond1) cv 
        then True 
        else False 

      constRight1 = if hasCv (getRightExp cond1) cv 
        then True 
        else False

      dep = if constLeft || constRight 
        then True 
        else False
      
      --const din Garda 2
      constG = if constLeft == True && gt == G_VC 
        then getConst (getLeftExp cond1)
        else if constRight == True && gt == G_VC 
          then getConst (getRightExp cond1)
        else 0 


      -- const din g1
      constG1 = if constLeft == True && gt1 == G_VC 
        then getConst (getLeftExp cond1)
        else if constRight == True && gt1 == G_VC 
          then getConst (getRightExp cond1)
        else 0

      
      constExp = getConst exp --default 0 la getConst daca nu are constanta
      --garda 2 cu assignment 1
      opposedAssignment = if (constLeft || constRight) && gt == G_VC && op == OP_VC
        then ((gop cond && constG == constExp) || (gopEq cond && constG /= constExp)) -- && isDefClear(index1, index2, index var curenta)
        else False

      --garda 2 cu garda 1
      opposedGuard = if (constLeft1 || constRight1) && (constLeft || constRight) && gt1 == G_VC -- && isDefClear(index1, index2, index var curenta)
        then ((constG == constG1) && (gop1 cond cond1 || gop1 cond1 cond || gop2 cond cond1 || gop2 cond1 cond || gop3 cond cond1 || gop3 cond1 cond)) || ((constG /= constG1) && (gopEq cond && gopEq cond1))
        else False

      -----
      opposed = opposedGuard || opposedAssignment

      --penalty-urile variabilelor (ar trebui tot gp si sa mai pun o conditie in if si sa las un singur gp = ...?)
      penaltyLocal = getPenalty cond exp opposed

      vp = if constLeft || constRight
        then penaltyLocal
        else 0

-------------------------------------------------

--TrA [[(Int, Int), (Int,Int), (Int,Int)...], Int, Bool]
getTransitionArray :: Transition -> Transition -> Int -> Int -> TransArray 
getTransitionArray tr1 tr2 iT1 iT2 = TrA penalties gp dep 
    where 
        g1 = condition tr1
        g2 = condition tr2
        assignments1 = operations tr1
        variables = vars efsm

        --V var scot constructorul
        penaltiesAux = [ computePenalty var  | var <- variables ]

        ------
        penalties = [(vp, penalty) | (vp, penalty, _) <- penaltiesAux ]

        dep = or [d | (_, _, d) <- penaltiesAux]
        ------
        --cv = current variable, trebuie parcurse toate var din efsm pt fiecare pereche de tranzitii
        computePenalty cv =  (varP, vp, dep)
            where 
                --penalty = 0
                l = [exp | Atrib v exp <- assignments1, v == cv]
                varP = if length l == 0 
                        then 0
                        else let expr = head l 
                                 op = getOpType expr in
                                    case op of 
                                        OP_VC -> -2
                                        OP_PV -> -1
                                        OP_VV -> getVarIndexFromAssignment cv
                (vp, dep) = if length l == 0 
                        then (0, False)
                        else let expr = head l in 
                          penaltiesValues g2 g1 expr cv 
        gp = guardPenaltyValue g2 NilE           
                
----------------------------------------------------------------------------------------------------

--conditia din guard t2
--e atrib t1

      
--prima atribuire din ti care contine var curenta
--pt fiecare var 


--la getTransitionArray luam doar pt o singura variabila, cazul v1 + v2 > const nu e acoperit
---------------------------------------------------------------
