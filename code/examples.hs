module Examples where
import EFSM_2
import GA

--p1 = (P [Transition "t0" (S "s0") (S "s1") [] [] [], Transition "t1" (S "s1") (S "s0") [] [] [], Transition "t0" (S "s0") (S "s1") [] [] []] )

--p2 = P [Transition "t0" (S "s0") (S "s1") [] [] [], Transition "t1" (S "s1") (S "s0") [] [] [], Transition "t1" (S "s1") (S "s0") [] [] []] 


--ex (P [Transition "t1" (S "s1") (S "s1") (Gte (Param "p1") (Const 10) :&: Lte (Param "p1") (Const 20) :&: Gte (Param "p2") (Const 0) :&: Lte (Param "p2") (Const 10)) [I "aa"] [Atrib (V "v1") (Param "p1"), Atrib (V "v2") (Param "p2")])
testnOT = nOT (S "s1")

ch1 :: Chromosome
ch1 = [[1,1,1,1], [1,2,1,2]]
--ch1 = 

chToPath :: Path
chToPath = chromosomeToPath (S "s1") ch1

validChToPath = isValid chToPath