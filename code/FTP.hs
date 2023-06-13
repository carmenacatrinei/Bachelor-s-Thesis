module FTP where
import Helpers
import EFSM_2 
import Inputs
---------------------------------------


--Penalties values for variables
penaltiesValues :: Path -> Int -> Int -> Condition -> Condition -> ExpAr -> VarMem -> (Int, Dependency)
penaltiesValues _ _ _ Nil _ _ _ = (0, False)
penaltiesValues _ _ _ T _ _ _ = (0, False)
penaltiesValues _ _ _ F _ _ _ = (0, False)
penaltiesValues p index1 index2 (c1 :&: c2) c3 e v = (vp1+vp2, dep1 || dep2)
    where (vp1,dep1) = penaltiesValues p index1 index2 c1 c3 e v
          (vp2,dep2) = penaltiesValues p index1 index2 c2 c3 e v
penaltiesValues p index1 index2 (c1 :|: c2) c3 e v = (min vp1 vp2, dep1 || dep2)
    where (vp1,dep1) = penaltiesValues p index1 index2 c1 c3 e v
          (vp2,dep2) = penaltiesValues p index1 index2 c2 c3 e v 
penaltiesValues p index1 index2 c c3 e v = localPenalty p index1 index2 c c3 e v

--localPenalty pentru variabile
localPenalty :: Path -> Int -> Int-> Condition -> Condition -> ExpAr -> VarMem -> (Int, Dependency) --Int pentru penalty-ul fiecarei variabile
localPenalty p index1 index2 cond cond1 exp cv = (vp, dep)
    where
      gt = getGType cond --DIN AFFECTED-BY
      gt1 = getGType cond1 --DIN AFFECTING
      op = getOpType exp --DIN AFFECTING
      --index1 = --affecting
      --index2 = --affected-by

      --VERIFICA VAR CURENTA
      constLeft = if hasCv (getLeftExp cond) cv 
        then True 
        else False 

      constRight = if hasCv (getRightExp cond) cv 
        then True 
        else False

      constLeft1 = if hasCv (getLeftExp2 cond1) cv 
        then True 
        else False 

      constRight1 = if hasCv (getRightExp2 cond1) cv 
        then True 
        else False
      -----

      dep = if constLeft || constRight 
        then True 
        else False
      
      --const din G2
      constG = if constLeft == True && gt == G_VC 
        then getConst (getRightExp cond) --inversate aici
        else if constRight == True && gt == G_VC 
          then getConst (getLeftExp cond) --inversate getLeftExp cu getRightExp si mai jos la fel
        else 0

      --const din G1
      constG1 = if constLeft == True && gt1 == G_VC 
        then getConst (getRightExp2 cond1)
        else if constRight == True && gt1 == G_VC 
          then getConst (getLeftExp2 cond1)
        else 0

      constExp = getConst exp --default 0 la getConst daca nu are constanta

      --garda 2 cu assignment 1
      opposedAssignment = if (constLeft || constRight) && gt == G_VC && op == OP_VC
        then ((gop cond && constG == constExp) || (gopEq cond && constG /= constExp)) && isDefClear p index1 index2 cv
        else False

      --garda 2 cu garda 1
      opposedGuard = if (constLeft1 || constRight1) && (constLeft || constRight) && gt1 == G_VC && isDefClear p index1 index2 cv
        then ((constG == constG1) && (gop1 cond cond1 || gop1 cond1 cond || gop2 cond cond1 || gop2 cond1 cond || gop3 cond cond1 || gop3 cond1 cond)) || ((constG /= constG1) && (gopEq cond && gopEq cond1))
        else False

      -----
      opposed = opposedGuard || opposedAssignment

      --penalty-urile variabilelor 
      penaltyLocal = getPenalty cond exp opposed

      vp = if constLeft || constRight
        then penaltyLocal
        else 0

------------------------------------------------------------------------------

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
guardPenaltyValue cond exp = guardPenaltyValueAux cond exp

  
guardPenaltyValueAux :: Condition -> ExpAr -> Gp  
guardPenaltyValueAux cond exp = gp  
  where
     penaltyLocal = getPenalty cond exp False 
     gt = getGType cond
     gp = if gt == G_PC || gt == G_PP
          then penaltyLocal
          else 0

-------------------------------------------------

--TrA [[(Int, Int), (Int,Int), (Int,Int)...], Int, Bool]
getTransitionArray :: Path -> Transition -> Transition -> Int -> Int -> TransArray 
getTransitionArray p tr1 tr2 iT1 iT2 = TrA nameTr penalties gp dep 
  --e
    where 
        g1 = condition tr1 --tr1 = affected-by
        g2 = condition tr2 --tr2 = affecting
        assignments1 = operations tr2 -- DIN AFFECTING
        variables = vars efsm
        name1 = name tr1 --affected-by
        name2 = name tr2 --affecting
        nameTr = name1 ++ name2

        --V var scot constructorul
        penaltiesAux = [ computePenalty var  | var <- variables ]

        ------
        penalties = [(varP, vp) | (varP, vp, _) <- penaltiesAux ]
        --ex afisare
        --e = error (show penalties)
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
                                        OP_VV -> getVarIndexFromAssignment2 expr
                (vp, dep) = if length l == 0 
                        then (0, False)
                        else let expr = head l in 
                          penaltiesValues p iT1 iT2 g1 g2 expr cv --am schimbat 1 cu 2
        gp = guardPenaltyValue g1 NilE     --inversat g2 cu g1      
                
----------------------------------------------------------------------------------------------------

--pe diagonala
getTransitionMatrix :: Path -> [TransArray]
getTransitionMatrix (P p) = [getTransitionArray (P p) tr1 tr i j| i <- [0..length p - 1], j <- [i ..length p - 1], let tr1 = p!!j , let tr = p!!i  ]

------------------------------------------------------------------

isDefClear :: Path -> Int -> Int -> VarMem -> Bool 
isDefClear (P p) iT1 iT2 cv = result
  where 
    result = and [isDefClearAux pi | pi <- [iT1..iT2 - 1]]
    isDefClearAux pi = auxRes 
      where
        --tranzitia de pe pozitia pi din path
        nameTr = [name tranP | tranP <- p] !! pi
        tr = head [tran | tran <- transitions efsm, nameTr == name tran]

        assignmentsTr = [assignment | assignment <- operations tr]
        auxRes = and [cv == aVar | Atrib aVar exp <- assignmentsTr]


---------------------------------

--compute function

--i affected-by
--j affecting

compute :: Path -> Int
compute (P ftp) = result1 + auxResults
  where
    matrix = getTransitionMatrix (P ftp)
    -- varsArray = [False | v <- vars efsm]

    result1 = if isValid (P ftp) == False
                then inf 
                else 0 
    
    n = length ftp
    
    auxResults = sum [getAuxResult pi | pi <- [n - 1, n - 2..1]]
    getAuxResult pi = auxRes + listAuxRes [False | v <- vars efsm] (pi - 1) 0
        where 
          varsArray = [False | v <- vars efsm]
          pj = pi 
          auxRes = getGp (getTrA (P ftp) matrix pi pj)
          -- listAuxRes = [[ | varBool <- varsArray, varBool == False] | j <-[pj-1..0], let trA =  getTrA (P ftp) matrix pi j, getDependecy trA == True ]
          listAuxRes varsArray 0 resList = resList 
          listAuxRes varsArray j resList = 
            let trA =  getTrA (P ftp) matrix pi j
              in 
                if getDependecy trA == True
                  then 
                    let varsArrayAux = [func vs e varBool | (varBool, vs) <- zip varsArray [0..], let e = (getVarPenalty trA) !! vs]
                        func vs e varBool = 
                          if fst e < 0 && varBool == False 
                            then (True, if snd e > 0 then snd e else 0)
                            else 
                              if fst e == 0 && varBool == False && snd e > 0 
                                then (False, snd e)
                                else if fst e == 0 && varBool == False
                                  then (False, 0)
                                else if fst e > 0 && varBool == False 
                                  then (True, if snd e > 0 then snd e + check (P ftp) pi j vs else 0)
                                else 
                                  (varBool, 0)     
                        varsArray1 = map fst varsArrayAux 
                        resList1 = resList + sum (map snd varsArrayAux)
                        
                      in listAuxRes varsArray1 (j - 1) resList1
                  else listAuxRes varsArray (j - 1) resList     


--returneaza tranzitia de pe pozitia i j din matrice
getTrA :: Path -> [TransArray] -> Int -> Int -> TransArray
getTrA (P ftp) matrix pi pj = trA 
          where 
            name1 = name (ftp !! pi) --affected-by cu i
            name2 = name (ftp !! pj) --affecting cu j
            nameTr = name1 ++ name2
            trA = head [trA | trA <- matrix, nameTrA trA == nameTr]

    
check :: Path -> Int -> Int -> Int -> Int
check p pi pj vs = 
  let result = 0
      found = False
      k = pj + 1
      matrix = getTransitionMatrix p
      checkAux p k result found = 
        if k > 0 && not found 
          then let kp = k - 1 
                   trA = getTrA p matrix pi pj
                   opIntType = fst ((getVarPenalty trA) !! vs )
                in if opIntType /= 0 
                   then 
                    if opIntType == -2 
                      then checkAux p kp (result + 60) True 
                      else if opIntType == -1 
                        then checkAux p kp (result + 20) True 
                        else checkAux p kp (result + 40 + (check p kp (kp - 1) (opIntType - 1))) True
                  else checkAux p kp result False
        else if found == True 
          then result
        else result + 60
  in checkAux p k result found


--conditia din guard t2
--e = atrib t1

--prima atribuire din ti care contine var curenta
--pt fiecare var 


--la getTransitionArray luam doar pt o singura variabila, cazul v1 + v2 > const nu e acoperit
---------------------------------------------------------------
