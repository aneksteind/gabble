{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GA
    ( 
        GAConfig (..),
        GASnapshot (..),
        GAContext (..),
        HOF,
        random,
        randomD,
        randomW,
        randomBool,
        mutateBool,
        runGASeed,
        evalGASeed,
        evalGA,
        logNothing,
        logHOF
    ) where

import Recursive
import Control.Monad ((>>))
import Control.Monad.RWS.Lazy (RWS, rws, runRWS, evalRWS, ask, tell, MonadReader, MonadWriter)
import Data.Functor.Foldable (Fix (..), ListF (..), cata, embed)
import Data.Functor.Foldable.Exotic (cataM, anaM)
import Data.Fix (hyloM)
import Data.List (sort)
import Data.Bits ((.&.))
import qualified Data.Text as T
import qualified Data.Heap as Heap
import System.Random.Mersenne.Pure64 (randomInt, pureMT, PureMT, newPureMT, randomDouble, randomWord)

-- the Hall Of Fame is min-heap of the best individuals
type HOF a = Heap.MinHeap a

newtype GAContext i a = GAContext {
    ctx :: RWS (GAConfig i) [T.Text] PureMT a
}    deriving (Functor, Applicative, Monad, MonadReader (GAConfig i), MonadWriter [T.Text])

data GAConfig i = Config {
    mutationRateInd :: Double -- the probability an individual is mutated
  , mutationRateChr :: Double -- the probability a chromosome of an individual is mutated
  , crossoverRate :: Double -- the percentage of the population that gets replaced through recombination
  , popSize :: Int -- the population size
  , mutate :: Double -> i -> GAContext i i -- the mutation method
  , crossover :: i -> i -> GAContext i i -- the crossover method
  , randomIndividual :: GAContext i i  -- the method to create a new individual
  , selectionMethod :: [i] -> GAContext i [i] -- the selection method
  , fitness :: i -> Double -- the fitness function (higher fitness is preferred)
  , numGenerations :: Int -- the number of generations
  , hofSize :: Int -- the `hofSize` best individuals across all generations
  , logFunc :: GASnapshot i -> GAContext i () -- function for information sourced from most recent snapshot
}

data GASnapshot a = Snapshot {
    lastGeneration :: [a]
  , hof :: HOF a -- the list of top performers, the Hall of Fame (HOF)
  , generationNumber :: Int
} deriving (Show)

random :: (PureMT -> (a, PureMT)) -> GAContext b a
random f = GAContext $ rws (\_ s -> let (a,s') = f s in (a, s', []))

randomD :: GAContext a Double
randomD = random randomDouble

randomW :: GAContext a Word
randomW = random randomWord

randomBool :: GAContext a Bool
randomBool = do
    x <- randomW
    return $ x .&. 1 /= 0

-- mutate a boolean by flipping it
mutateBool :: Double -> Bool -> GAContext a Bool
mutateBool p x = do
    indp <- randomD
    if indp < p then return $ not x else return x

toList :: AlgebraM (GAContext a) (ListF a) [a]
toList = return . embed

makePopulation :: Int -> GAContext a [a]
makePopulation s = hyloM toList randomInd s where
    randomInd :: CoAlgebraM (GAContext a) (ListF a) Int
    randomInd 0 = return Nil
    randomInd n = do
        cfg <- ask
        ind <- randomIndividual cfg
        return $ Cons ind (n-1)

-- mutates individuals in the population with probability `p`
mutatePop :: Double -> Fix (ListF a) -> GAContext a [a]
mutatePop p pop = hyloM toList abb pop where
    abb :: CoAlgebraM (GAContext a) (ListF a) (Fix (ListF a))
    abb (Fix Nil) = return Nil
    abb (Fix (Cons a mutated)) = do
        cfg <- ask
        m <- (mutate cfg) p a
        return $ Cons m mutated      

-- repeatedly selects two new parents from `parents` from which `n` total children are produced
reproduceFrom :: (Fix (ListF a)) -> Int -> GAContext a (Fix (ListF a))
reproduceFrom parents n = anaM b n where
    b 0 = return Nil
    b m = do
        cfg <- ask

        randInt1 <- random randomInt
        randInt2 <- random randomInt

        let p1 = parents !!! (randInt1 `mod` (lengthF parents))
        let p2 = parents !!! (randInt2 `mod` (lengthF parents))

        child <- (crossover cfg) p1 p2
        return $ Cons child (m-1)

-- selects a number of individuals from the current population to create the next population
select :: Ord a => [a] -> GAContext a (Fix (ListF a))
select pop = do
    cfg <- ask
    selected <- (selectionMethod cfg) pop
    cataM (return . Fix) selected


-- repeatedly selects two new parents from `parents` from which `n` total children are produced
cross :: Ord a => Fix (ListF a) -> GAContext a (Fix (ListF a))
cross parents = do
    cfg <- ask
    children <- reproduceFrom parents (popSize cfg) 
    return children

-- inserts elements from a list into a heap
insertHeap :: Ord a => HOF a -> Fix (ListF a) -> HOF a
insertHeap hof = cata insert where
    insert Nil = hof
    insert (Cons a heap) = Heap.insert a heap

-- updates the HOF by removing the worst-fit individuals from the min-heap
updateHOF :: Ord a => GASnapshot a -> [a] -> Int -> GAContext a (GASnapshot a)
updateHOF snapshot pop hofSize = return $ snapshot { hof = newHOF } where
    currentHOF = hof snapshot
    newHOF = case Heap.isEmpty currentHOF of
        -- initialize the HOF with the `hofSize` best individuals
        True -> Heap.fromAscList . take hofSize . sort $ pop
        -- update the HOF with the best performers seen thus far
        False -> Heap.drop (length pop) $ foldr (Heap.insert) currentHOF pop

logNothing :: b -> GAContext a ()
logNothing _ = return ()

logHOF :: Ord a => GASnapshot a -> GAContext a ()
logHOF snap = do
    cfg <- ask
    let currentGen = T.pack . show $ generationNumber snap
    let best = T.pack . show . map (fitness cfg) . Heap.toList $ hof snap :: T.Text
    let msg = T.concat ["best individuals as of generation ", currentGen, ": ", best]
    tell [msg]

step :: Ord a => GASnapshot a -> GAContext a (GASnapshot a)
step snapshot = do
    cfg <- ask 
    -- select parents and create the next generation from them
    selectedParents <- select $ lastGeneration snapshot
    -- use the set of parents to create a new generation
    crossed <- cross selectedParents
    -- mutate the next generation
    mutated <- mutatePop (mutationRateInd cfg) crossed
    -- update the HOF
    nextSnapshot <- updateHOF snapshot mutated (hofSize cfg)
    -- log intermediate results
    let log = logFunc cfg
    log nextSnapshot
    -- return the mutated generation
    return $ nextSnapshot {
        lastGeneration = cata embed mutated,
        generationNumber = (generationNumber snapshot) + 1
    }

-- a function reminiscent of iterateM that completes
-- after `n` evaluations, returning the `n`th result
runN :: Monad m => Int -> (a -> m a) -> a -> m a
runN 0 _ a = return a
runN n f a = do
    a' <- f a
    runN (n-1) f a'

runGA' :: Ord a => GAContext a (GASnapshot a)
runGA' = do
    cfg <- ask
    -- initialize the population
    initialPop <- makePopulation (popSize cfg)
    -- set up the initial result
    let snapshot = Snapshot {
                lastGeneration = initialPop,
                hof = Heap.empty :: HOF a,
                generationNumber = 0
              }
    -- run the genetic algorithm
    runN (numGenerations cfg) step snapshot

evalGA :: Ord i => GAConfig i -> IO (GASnapshot i, [T.Text])
evalGA cfg = do
    rng <- newPureMT
    return $ evalGASeed cfg rng

evalGASeed :: Ord i => GAConfig i -> PureMT -> (GASnapshot i, [T.Text])
evalGASeed cfg rng = evalRWS (ctx runGA') cfg rng

runGASeed :: Ord i => GAConfig i -> PureMT -> (GASnapshot i, PureMT, [T.Text])
runGASeed cfg rng = runRWS (ctx runGA') cfg rng
