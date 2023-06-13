module GA where
import Moo.GeneticAlgorithm.Multiobjective
import Moo.GeneticAlgorithm.Continuous
import System.IO.Unsafe
import System.Random (randomR, mkStdGen, randomRs, newStdGen)
import System.Random.Mersenne.Pure64
import Moo.GeneticAlgorithm.Random
import EFSM_2
import Data.List
import FTP
import Helpers
import Inputs

----TIME----

---END TIME----


---------------------------GA---------------------------
--from library

---time
--rulez pe 1000 generatii cu 50 popsize si mai multe variatii

type Chromosome = Genome Gene --[[]]
type Gene = Genome Int --[]

popsize = 20
generations = 50

--end from library

--fitness function
--suma din (1-val) / inf, val e compute de ch to path, suma pt fiecare gena din cromozom 

chrGenes :: Chromosome -> [Gene]
chrGenes chr =  [ gene | gene <- chr]

roundDec :: Double -> Double 
roundDec x = (fromIntegral $ round (x * 1000)) / 1000.0

--ce stare dau? 
--nu stiu exact cum ar trebui testat, dar pare sa functioneze (poate da si valori negative??) 
--rotunjire la 4 zecimale
fitness1 :: Chromosome -> Double 
fitness1 chr = roundDec $ sum (complexity chr)

complexity chr =  [if val > inf then fromIntegral val else (1 - ((fromIntegral val :: Double) / fromIntegral inf)) | gene <- chrGenes chr, let val = compute (geneToPath (start efsm) gene)]

--un cr are mai multe cai
--fitness1 complexitate
--fitness2 acoperire tranzitii
--test
g1:: Chromosome
g1 = [[0,1,0,0]]

--complexitate
--cu cat sunt mai rele caile, cu atat valoarea da mai mare
--vreau sa fie cat mai mare, dar nu inf
-- 

--compute 3 din EFSM Parsing
         
fitness2 :: Chromosome -> Double      
fitness2 chr = (roundDec resultFinal) + fromIntegral zeros 
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
                            then 10000  
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
                pen = computePenaltiesAux tp pen1
                computePenaltiesAux (P []) pc = pc 
                computePenaltiesAux (P(x:xs)) pc = computePenaltiesAux (P xs) (func tran pc x )
                func [] [] x = []
                func (x:xs) (y:ys) z = [if z == x then y + 1 else y] ++ func xs ys z

--mop 
mop = [(Minimizing, fitness1), (Minimizing, fitness2)]

--initializare
--genereaza o lista de lungime random de liste de lungime stabilita initial 
--fiecare sublista are intregi din intervalul [1, lcm - 1]
--ArraySolution.java din ga, liniile 45-70

upperBound :: Int 
upperBound = lcmnumberOfTransitions - 1

pathSize :: Int
pathSize = (length transition) `div` 2

maxSize :: Int
maxSize = numberOfTotalTr

minSize :: Int
minSize = 1

--randomSize :: Rand Int 
randomSize = getRandomR (5, maxSize)

--randomGen :: Rand Int
randomGen =  getRandomR (0, upperBound) 

--generateRandomGene :: Rand [Gene]
generateRandomCh = getRandomGenomes pathSize [(0, upperBound) | i <- [1..randomInt2 5 maxSize]]

--randomInt = unsafePerformIO (randomRIO (5, maxSize))
randomInt2 x y = unsafePerformIO (nextInt2 x y)



nextInt2 :: Int -> Int -> IO Int 
nextInt2 x y = do 
    g <- newStdGen
    return  $ fst (randomR (x, y) g)

-- initial :: Rand [Gene]
-- initial = do 

initialize :: Rand [Chromosome] 
initialize = sequence [generateRandomCh | j <- [1..popsize]]
--type MutationOp a = Genome a -> Rand (Genome a)
-- mapM :: (a-> Rand b) -> Genome a -> Rand(Genome b)
-- mutation2 :: b -> Rand c 
-- f :: (b -> r c) -> (r (m b)) -> r m c 
myMutation :: Double  -- ^ probability @p@
            -> MutationOp Gene
myMutation p vars =  mapM mutate vars
  where
    mutate = withProbability p f 
    f v = sequence [getRandomR (0, upperBound) | i <- [1..randomInt2 1 pathSize]] 
            
--stop, popstate? 
step = stepNSGA2bt mop (onePointCrossover 0.5) (myMutation 0.75) 

--crossover
-- type CrossoverOp a = [Genome a] -> Rand ([Genome a], [Genome a])
-- -- | A mutation operator takes a genome and returns an altered copy of it.
--0.5 prob 0.2 sigma
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

---------------getName-------------
pathToString :: Path -> String
pathToString (P p) = intercalate "-" $ map name p 


---------------------MAIN--------------------------
main = do
  result <- runGA initialize $ loop (Generations generations) step
  let solutions = evalAllObjectives mop $  map takeGenome $ result 
  
  let mapSol ch = [ (pathToString p , if val > inf then fromIntegral val else (1 - ((fromIntegral val :: Double) / fromIntegral inf)) ) | g <- ch, let p = geneToPath (start efsm) g, let val = compute p ]
  
  let filterSol = [(mapSol ch , objs) | (ch, objs) <- solutions, objs !! 1 < 1, objs!! 0 < fromIntegral inf]

  putStrLn $ show filterSol

  
  -- let solutions = map takeGenome $ takeWhile ((<=10.0) . takeObjectiveValue) result
--   let ovs = map takeObjectiveValues $ evalAllObjectives mop solutions
--   flip mapM_ ovs $ \[x1,x2] ->
--       putStrLn $ show x1 ++ "\t" ++ show x2 
--   let bestpath = map (map (pathToString.(geneToPath (start efsm)))) solutions
--   putStrLn $ show bestpath



