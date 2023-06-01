module GA where
import Moo.GeneticAlgorithm.Continuous
import EFSM_2
import Data.List
import FTP
import Helpers

---------------------------GA---------------------------
--from library

type Chromosome = Genome Gene
type Gene = Genome Int 

--end from library

--fitness function
--suma din (1-val) / inf, val e compute de ch to path, suma pt fiecare gena din cromozom 

chrGenes :: Chromosome -> [Gene]
chrGenes chr =  [ gene | gene <- chr]

--ce stare dau? 
--nu stiu exact cum ar trebui testat, dar pare sa functioneze (poate da si valori negative??) 
--doar cu 0 si 1 merge?
fitness1 :: Chromosome -> Double 
fitness1 chr = sum [((fromIntegral (1 - val) :: Double) / fromIntegral inf) | gene <- chrGenes chr, let val = compute (geneToPath (start efsm) gene)]

--compute 3 din EFSM Parsing
         
fitness2 :: Chromosome -> Double      
fitness2 chr = resultFinal + fromIntegral zeros 
    where 
        result1 = countRes chr
        penalties = computePenalties chr penaltiesAux
        --penalties = [0 | i <- [1..length transition]]
        zeros = length [p | p <- penalties, p == 0]
        resultFinal = norm (fromIntegral result1)

        countRes [] = 0
        countRes (gene:chr) = res + countRes chr
                where
                    tp = geneToPath (start efsm) gene
                    res = if countResAux tp == 0 
                            then 1000  
                            else countResAux tp 

        countResAux :: Path -> Int
        countResAux (P []) = 0
        countResAux (P tp) = sum [1 | t <- tp] 

        tran = transition
        --penalties = [val | val <- ]
        penaltiesAux = [0 | i <- [1..length transition]]
        computePenalties [] p = p
        computePenalties (gene:chr) pen1 = computePenalties chr pen
            where 
                tp = geneToPath (start efsm) gene
                pen = computePenaltiesAux tp
                computePenaltiesAux (P(x:xs)) = func tran pen1 x 
                func [] [] x = []
                func (x:xs) (y:ys) z = [if z == x then y + 1 else y] ++ func xs ys z


--selection op

--mop 
--mop = [(Minimizing, fitness1), (Minimizing, fitness2)]

--initializare
--genereaza o lista de lungime random de liste de lungime stabilita initial 
--fiecare sublista are intregi din intervalul [1, lcm - 1]
--ArraySolution.java din ga, liniile 45-70
initial = []

--crossover
-- type CrossoverOp a = [Genome a] -> Rand ([Genome a], [Genome a])

-- -- | A mutation operator takes a genome and returns an altered copy of it.
-- type MutationOp a = Genome a -> Rand (Genome a)

--mutation

-------------------------END GA-------------------------

----------------------Solution Encoding---------------------
geneToPath :: State -> Gene -> Path
geneToPath (S st) [] =  P []
geneToPath (S st) (g:gene) = P (t : x)
                                        where
                                            P x = geneToPath st2 gene
                                            m = g `div` getRangeForState (S st)
                                            t = leavingStateS (S st) !! m
                                            st2 = s2 (getmThTransLeavingStateS (S st) m)

----------------------END Solution Encoding---------------------