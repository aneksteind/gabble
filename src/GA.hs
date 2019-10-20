{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GA
    ( 
        GAConfig (..),
        GASnapshot (..),
        GAContext (..),
        SelectionMethod (..),
        HOF,
        random,
        runGA
    ) where

import Recursive
import Control.Monad.RWS.Lazy (RWS, rws, ask, tell, MonadReader, MonadWriter)
import Data.Functor.Foldable (Fix (..), ListF (..), cata, hylo, embed)
import Data.Functor.Foldable.Exotic (cataM, anaM)
import Data.Fix (hyloM)
import qualified Data.Text as T
import qualified Data.Heap as Heap
import System.Random.Mersenne.Pure64 (randomInt, PureMT)

-- the Hall Of Fame is min-heap of the best individuals
type HOF a = Heap.MinHeap a

data SelectionMethod = Tournament Int -- | more eventually...

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
  , selectionMethod :: SelectionMethod -- the selection method
  , fitness :: i -> Double -- the fitness function (higher fitness is preferred)
  , numGenerations :: Int -- the number of generations
}

data GASnapshot a = Snapshot {
    lastGeneration :: [a]
  , hof :: HOF a -- the list of top performers, the Hall of Fame (HOF)
  , hofSize :: Int -- max size of the HOF
} deriving (Show)

-- creates a population of size `popSize`
-- makePopulation :: Int -> GAContext a (Fix (ListF a))
-- makePopulation size = anaM makeInd size

makeIndList :: Int -> GAContext a [a]
makeIndList s = hyloM cat ann s where
    cat :: AlgebraM (GAContext a) (ListF a) [a]
    cat = (return . embed)

    ann :: CoAlgebraM (GAContext a) (ListF a) Int
    ann 0 = return Nil
    ann n = do
        cfg <- ask
        ind <- randomIndividual cfg
        return $ Cons ind (n-1)

-- mutates individuals in the population with probability `p`
mutatePop :: Double -> Fix (ListF a) -> GAContext a (Fix (ListF a))
mutatePop p pop = do cataM abb pop where
    abb Nil = return nil
    abb (Cons a mutated) = do
        cfg <- ask
        m <- (mutate cfg) p a
        return $ cons m mutated

random :: (PureMT -> (a, PureMT)) -> GAContext b a
random f = GAContext $ rws (\_ s -> let (a,s') = f s in (a, s', []))        

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
select :: Ord a => Fix (ListF a) -> GAContext a (Fix (ListF a))
select pop = do -- TODO: add selection method here
    cfg <- ask
    let numToSelect = round $ (1.0 - crossoverRate cfg) * (fromIntegral $ popSize cfg)

    -- eventually... selectedParents = (selectionMethod cfg) numToSelect pop
    let selectedParents = takeF numToSelect $ sortF pop

    return selectedParents

-- repeatedly selects two new parents from `parents` from which `n` total children are produced
cross :: Ord a => Fix (ListF a) -> GAContext a (Fix (ListF a))
cross parents = do -- TODO: add crossover method here
    cfg <- ask
    children <- reproduceFrom parents (popSize cfg) 
    return children

-- inserts elements from a list into a heap
insertHeap :: Ord a => HOF a -> Fix (ListF a) -> HOF a
insertHeap hof = cata insert where
    insert Nil = hof
    insert (Cons a heap) = Heap.insert a heap

-- updates the HOF by removing the worst-fit individuals from the min-heap
updateHOF :: Ord a => GASnapshot a -> Fix (ListF a) -> GASnapshot a
updateHOF res pop = res { hof = newHOF } where
    currentHOF = hof res
    newHOF = case (Heap.isEmpty currentHOF) of
        -- initialize the HOF with the `hofSize` best individuals
        True -> insertHeap currentHOF $ takeF (hofSize res) pop
        -- update the HOF with the best performers seen thus far
        False -> Heap.drop (lengthF pop) $ insertHeap currentHOF pop

step :: Ord a => GASnapshot a -> GAContext a (GASnapshot a)
step snapshot = do
    cfg <- ask
    let pop = lastGeneration snapshot
    let score = fitness cfg
    -- select parents and create the next generation from them
    selectedParents <- select $ cata Fix pop
    -- use the set of parents to create a new generation
    crossed <- cross selectedParents
    -- mutate the next generation
    mutated <- mutatePop (mutationRateInd cfg) crossed
    -- update the HOF
    let newCtx = updateHOF snapshot mutated
    -- show intermediate results
    let best = (T.pack . show . reverse . map score . Heap.toList $ hof newCtx) :: T.Text
    let msg = T.concat ["best: ", best]
    tell $ ([msg] :: [T.Text])
    -- return the mutated generation
    return $ newCtx {lastGeneration = cata embed mutated}

-- a function reminiscent of iterateM that completes
-- after `n` evaluations, returning the `n`th result
runN :: Monad m => Int -> (a -> m a) -> a -> m a
runN 0 _ a = return a
runN n f a = do
    a' <- f a
    runN (n-1) f a'

runGA :: Ord a => GAContext a (GASnapshot a)
runGA = do

    cfg <- ask
    -- initialize the population
    initialPop <- makeIndList (popSize cfg)
    -- set up the initial result
    let snapshot = Snapshot {
                lastGeneration = initialPop,
                hof = Heap.empty :: HOF a,
                hofSize = 3
              }
    -- run the genetic algorithm
    final <- runN (numGenerations cfg) step snapshot
    -- get and return final information
    return final
