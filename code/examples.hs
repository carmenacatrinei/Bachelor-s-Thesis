module Examples where
import EFSM_2

p1 = P [Transition "t0" (S "s0") (S "s1") [] [] [], Transition "t1" (S "s1") (S "s0") [] [] [], Transition "t0" (S "s0") (S "s1") [] [] []] 

p2 = P [Transition "t0" (S "s0") (S "s1") [] [] [], Transition "t1" (S "s1") (S "s0") [] [] [], Transition "t1" (S "s1") (S "s0") [] [] []] 

testnOT = nOT (S "s1")

ch1 :: Chromosome
ch1 = [5,3,4,2,1,3]

chToPath :: Path
chToPath = chromosomeToPath (S "s1") ch1

validChToPath = isValid chToPath