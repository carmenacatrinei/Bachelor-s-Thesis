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