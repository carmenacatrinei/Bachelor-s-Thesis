module GA
 where
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
import System.IO

----TIME----
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

toMs :: TimeSpec -> Integer
toMs (TimeSpec sec nsec) =
   toInteger sec * 1000 + fromIntegral (nsec `div` 1000000)
---END TIME----

---------------------------GA---------------------------

type Chromosome = Genome Gene --[[]] 
type Gene = Genome Int --[]

popsize = 50
generations = 500

chrGenes :: Chromosome -> [Gene]
chrGenes chr =  [ gene | gene <- chr]

--round with 4 decs
roundDec :: Double -> Double 
roundDec x = (fromIntegral $ round (x * 100000)) / 100000.0

fitness1 :: Chromosome -> Double 
fitness1 chr = roundDec $ sum (complexity chr)

complexity chr =  [if val > inf then fromIntegral val else (1 - ((fromIntegral val :: Double) / fromIntegral inf)) | gene <- chrGenes chr, let val = compute (geneToPath (start efsm) gene)]

g1:: Chromosome
g1 = [[0,1,0,0]]

fitness2 :: Chromosome -> Double      
fitness2 chr = (roundDec resultFinal) + fromIntegral zeros 
 where 
    result1 = countRes chr
    penalties = computePenalties chr penaltiesAux
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

mop = [(Minimizing, fitness1), (Minimizing, fitness2)]

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

initialize :: Rand [Chromosome] 
initialize = sequence [generateRandomCh | j <- [1..popsize]]

myMutation :: Double 
            -> MutationOp Gene
myMutation p vars =  mapM mutate vars
  where
    mutate = withProbability p f 
    f v = sequence [getRandomR (0, upperBound) | i <- [1..randomInt2 1 pathSize]] 
            
step = stepNSGA2bt mop (onePointCrossover 0.5) (myMutation 0.75) 

stop = (GensNoChange 10 myEval Nothing) `Or` (Generations generations)

myEval :: [Objective] -> Int 
myEval objs = length $ filter (==1.0) objs

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


logStats = WriteEvery 1 $ \iterno pop -> [iterno]

---------------------MAIN--------------------------
main :: IO String
main = do
  startTime <- getTime Monotonic
  (result, generationCount) <- runGA initialize $ loopWithLog logStats stop step
  let solutions = evalAllObjectives mop $  map takeGenome $ result
  
  let mapSol ch = [ (pathToString p , if val > inf then fromIntegral val else (1 - ((fromIntegral val :: Double) / fromIntegral inf)) ) | g <- ch, let p = geneToPath (start efsm) g, let val = compute p ]
  
  let filterSol = [(mapSol ch , objs) | (ch, objs) <- solutions, objs !! 1 <= 1.0, objs!! 0 < fromIntegral inf]
  let success = 100.0 *  (fromIntegral (length filterSol)) / (fromIntegral (length result))
  --putStrLn $ show filterSol
  endTime <- getTime Monotonic
  let timeSpecs = diffTimeSpec startTime endTime
  let milliseconds = toMs timeSpecs
  --genCount, timespec, solutionscount
  --putStrLn $ show (length generationCount) ++ "," ++ show milliseconds ++ "," ++ show (length filterSol) 
  let output_string = show (length generationCount) ++ "," ++ show milliseconds ++ "," ++ show (length filterSol) 
  return output_string
--   putStrLn $ show timeSpecs
--   putStrLn $ show milliseconds ++ " ms"
--   putStrLn $ show (length filterSol) ++ "  = SUCCESS RATE"

mainX :: Int -> IO String
mainX 0 = return ""

mainX x = do
    putStrLn $ show x 
    res <- main
    -- putStrLn $ show x 
    -- mainAux 
    res2 <- mainX (x - 1)
    return (res ++ "\n" ++ res2) 
    
main_2 = do 
    output <- mainX 100
    writeFile "output.csv" output
