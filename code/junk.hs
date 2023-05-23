evalCondition :: Condition -> Bool
evalCondition (E e) = evalExpBool e
evalCondition (condition1 :&: condition2) = evalCondition condition1 && evalCondition condition2
evalCondition (condition1 :|: condition2) = evalCondition condition1 || evalCondition condition2


evalExpBool :: Condition -> Int-> Bool

evalExpBool ( Lt exp1 exp2) = evalExpAr exp1 < evalExpAr exp2
evalExpBool ( Gt exp1 exp2) = evalExpAr exp1 > evalExpAr exp2
evalExpBool ( Lte exp1 exp2) = evalExpAr exp1 <= evalExpAr exp2
evalExpBool ( Gte exp1 exp2) = evalExpAr exp1 >= evalExpAr exp2
evalExpBool ( Dif exp1 exp2) = evalExpAr exp1 /= evalExpAr exp2
evalExpBool (Eq exp1 exp2) = evalExpAr exp1 == evalExpAr exp2


evalExpAr :: ExpAr ->Int-> Int
evalExpAr (Cint x) i = x
evalExpAr (VarMem x) = i
evalExpAr (Input x) = x
evalExpAr (Op2 Minus exp1 exp2) = evalExpAr exp1 - evalExpAr exp2
evalExpAr (Op2 Mult exp1 exp2) = evalExpAr exp1 * evalExpAr exp2
evalExpAr (Op2 Div exp1 exp2) = evalExpAr exp1 `div` evalExpAr exp2
evalExpAr (Op2 Mod exp1 exp2) = evalExpAr exp1 `mod` evalExpAr exp2

{-

-}
--ce tranzitii pleaca din starea st

--for testing only
--leavingStateS :: State -> [String]
--leavingStateS (S st) = [name t | t <- transition, s1 t == S st]


--getmThTransLeavingStateS :: State -> Int -> String
--getmThTransLeavingStateS (S st) m = if m >= length (leavingStateS (S st))
--                                            then "there are less transitions/out of range"
--                                        else leavingStateS (S st) !! m


--incearca sa o faci altfel, primind ca parametrii o expresie si opposed si din expresie sa scoti GType si OpType
--calculeaza ce inseamna opposed!
--gresit, in niciun caz tipul Operations, trebuie scos din expresie operatorul!!!!!!!!
-- getPenalty :: GType -> OpType -> Operations -> String -> String -> Bool -> Int
-- getPenalty g a op _ _ opposed = penalty 
--   where
--     if opposed == True
--       then penalty = 10000
--     ---------G_PV
--     if g == G_PV 
--       then
--         if a == OP_PV
--           then
--             if op == Eq
--               then penalty = 8
--             else if op == Lt or op == Gt 
--               then penalty = 6
--             else if op == Lte or op == Gte 
--               then penalty = 4
--             else penalty = 2
--         else if a == OP_VV 
--           then
--             if op == Eq 
--               then penalty = 16
--             else if op == Lt or op == Gt
--               then penalty =  12
--             else if op == Lte or op == Gte 
--               then penalty = 8
--             else penalty = 4
--         else if a == OP_VC 
--           then
--             if op == Eq
--               then penalty = 24
--             else if op == Lt or op == Gt 
--               then penalty = 18
--             else if op == Lte or op == Gte 
--               then penalty = 12
--             else penalty = 6
--         else 
--           if op == Eq
--               then penalty = 4
--             else if op == Lt or op == Gt 
--               then penalty = 3
--             else if op == Lte or op == Gte 
--               then penalty = 2
--             else penalty = 1
--     ----------G_VV
--     else if g == G_VV 
--       then
--         if a == OP_PV
--           then
--             if op == Eq
--               then penalty = 20
--             else if op == Lt or op == Gt 
--               then penalty = 16
--             else if op == Lte or op == Gte 
--               then penalty = 12
--             else penalty = 8
--         else if a == OP_VV 
--           then
--             if op == Eq 
--               then penalty = 40
--             else if op == Lt or op == Gt
--               then penalty =  32
--             else if op == Lte or op == Gte 
--               then penalty = 24
--             else penalty = 16
--         else if a == OP_VC 
--           then
--             if op == Eq
--               then penalty = 60
--             else if op == Lt or op == Gt 
--               then penalty = 48
--             else if op == Lte or op == Gte 
--               then penalty = 36
--             else penalty = 24
--         else 
--           if op == Eq
--               then penalty = 16
--             else if op == Lt or op == Gt 
--               then penalty = 12
--             else if op == Lte or op == Gte 
--               then penalty = 8
--             else penalty = 4
--     ---------G_VC
--     else if g == G_VC 
--       then
--         if a == OP_PV
--           then
--             if op == Eq
--               then penalty = 30
--             else if op == Lt or op == Gt 
--               then penalty = 24
--             else if op == Lte or op == Gte 
--               then penalty = 18
--             else penalty = 12
--         else if a == OP_VV 
--           then
--             if op == Eq 
--               then penalty = 60
--             else if op == Lt or op == Gt
--               then penalty =  48
--             else if op == Lte or op == Gte 
--               then penalty = 36
--             else penalty = 24
--          --------OP_VC DE MODIFICAT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--         else if a == OP_VC 
--           then
--             penalty = 0
--         else 
--           if op == Eq
--               then penalty = 40
--           else if op == Lt or op == Gt 
--             then penalty = 32
--           else if op == Lte or op == Gte 
--             then penalty = 24
--           else penalty = 16
--     -----------G_PC
--     else if g == G_PC
--       then
--         if a == NOP 
--           then
--             if op == Eq
--               then penalty = 12
--             else if op == Lt or op == Gt 
--               then penalty = 8
--             else if op == Lte or op == Gte 
--               then penalty = 4
--             else penalty = 1
--         else penalty = 0
--     else 
--       if a == NOP 
--           then
--             if op == Eq
--               then penalty = 6
--             else if op == Lt or op == Gt 
--               then penalty = 4
--             else if op == Lte or op == Gte 
--               then penalty = 2
--             else penalty = 1
--       else penalty = 0

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

--get const from expAr
-- getConst :: Condition -> Int
-- getConst (Const i) = i 
-- getConst (Lt e1 e2) = getConstAux e1 e2
-- getConst (Gt e1 e2) = getConstAux e1 e2
-- getConst (Eq e1 e2) = getConstAux e1 e2
-- getConst (Dif e1 e2) = getConstAux e1 e2

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


localPenalty :: Condition -> (Gp, Dependency)
localPenalty c = (gp, dep)
                  where
                    dep = False
                    if getGType c == G_PV || getGType c == G_VC || getGType c == G_VV 
                      then 
                        gp = 0
                      else gp = 


penalty = if constLeft || constRight
then penaltyLocal
else 0

penaltiesValues :: Condition -> Condition -> ExpAr -> VarMem -> (Gp, Dependency, Int)
penaltiesValues Nil _ _ _ = (0, False, 0)
penaltiesValues T _ _ _ = (0, False, 0)
penaltiesValues F _ _ _ = (0, False, 0)
penaltiesValues (c1 :&: c2) c3 e v = (gp1+gp2, dep1 || dep2, vp1 + vp2)
    where (gp1,dep1, vp1) = localPenalty c1 c3 e v
          (gp2,dep2, vp2) = localPenalty c2 c3 e v
penaltiesValues (c1 :|: c2) c3 e v = (min gp1 gp2, dep1 || dep2, min vp1 vp2)
    where (gp1,dep1, vp1) = localPenalty c1 c3 e v
          (gp2,dep2, vp2) = localPenalty c2 c3 e v 
penaltiesValues c c3 e v = localPenalty c c3 e  v

-- --path-ul dat ca parametru e ftp-ul
-- check :: Path -> Int -> Int -> Int -> Int
-- check p pi pj vs = result + resultAux
--   where 
--     found = False 
--     k = pj + 1
--     matrix = getTransitionMatrix (P p)
--     result = 0
    
--     if k > 0 && found == False 
--       then resultAux 
--         where 
--           trA = getTrA (P p) matrix pi pj
--           opIntType = fst (getVarPenalty trA) !! vs 

--           if opIntType /= 0 
--             then 
--               found = true 
--               if opIntType == -2 
--                 then resultAux = 60
--                 else if opIntType == -1
--                   then resultAux = 20
--                   else resultAux = 20 + check(p - 1, p - 2, opIntType - 1) 
--             else 
--               resultAux = check(p - 1, p - 2, opIntType - 1) 

--           -- k = k - 1
--       else if found == True 
--         then result 
--         else result + 60