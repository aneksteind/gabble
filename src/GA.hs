{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GA
    ( 
        GAConfig (..),
        GASnapshot (..),
        GAContext (..),
        HOF,
        random,
        runGA,
        makePopulation
    ) where

import Recursive
import Control.Monad (replicateM)
import Control.Monad.RWS.Lazy (RWS, rws, ask, tell, MonadReader, MonadWriter)
import Data.List (sort)
import Data.Bifunctor (bimap)
import Data.Functor.Foldable (Fix (..), ListF (..), cata, unfix, ana, hylo)
import Data.Functor.Foldable.Exotic (cataM, anaM)
import qualified Data.Text as T
import qualified Data.Heap as Heap
import System.Random.Mersenne.Pure64 (randomInt, PureMT)

-- the Hall Of Fame is min-heap of the best individuals
type HOF a = Heap.MinHeap a


newtype GAContext i a = GAContext {
    ctx :: RWS (GAConfig i) [T.Text] PureMT a
}    deriving (Functor, Applicative, Monad, MonadReader (GAConfig i), MonadWriter [T.Text])

data GAConfig i = Config {
    mutationRateInd :: Double -- the probability an individual is mutated
  , mutationRateChr :: Double -- the probability a chromosome of an individual is mutated
  , crossoverRate :: Double -- the percentage of the population that gets replaced through recombination
  , popSize :: Int
  , mutate :: Double -> i -> GAContext i i
  , crossover :: i -> i -> GAContext i i
  , randomIndividual :: GAContext i i 
  , fitness :: i -> Double
  , numGenerations :: Int
}

data GASnapshot a = Snapshot {
    lastGeneration :: Fix (ListF a)
  , hof :: HOF a -- the list of top performers, the Hall of Fame (HOF)
  , hofSize :: Int -- max size of the HOF
} deriving (Show)

-- creates a population of size `popSize`
makePopulation :: Int -> GAContext a (Fix (ListF a))
makePopulation size = anaM makeInd size where
    makeInd :: CoAlgebraM (GAContext a) (ListF a) Int
    makeInd 0 = return Nil
    makeInd n = do
        cfg <- ask
        ind <- randomIndividual cfg
        return $ Cons ind (n-1)

mutatePop :: Double -> Fix (ListF a) -> GAContext a (Fix (ListF a))
mutatePop p pop = do cataM abb pop where
    abb Nil = return nil
    abb (Cons a mutated) = do
        cfg <- ask
        m <- (mutate cfg) p a
        return $ cons m mutated

avg :: [Double] -> Double
avg ds = total / num where
    total = sum ds
    num = fromIntegral $ length ds :: Double

random :: (PureMT -> (a, PureMT)) -> GAContext b a
random f = GAContext $ rws (\_ s -> let (a,s') = f s in (a, s', []))

-- bearChild :: [a] -> GAContext a a
-- bearChild parents = do
        

reproduceFrom :: (Fix (ListF a)) -> Int -> GAContext a (Fix (ListF a))
reproduceFrom parents n = anaM b n where
    b 0 = return Nil
    b m = do
        cfg <- ask
        let crossoverMethod = crossover cfg
        let numParents = lengthF parents

        randInt1 <- random randomInt
        randInt2 <- random randomInt

        let p1 = parents !!! (randInt1 `mod` numParents)
        let p2 = parents !!! (randInt2 `mod` numParents)

        c <- crossoverMethod p1 p2
        return $ Cons c (m-1)


selectAndCross :: Ord a => Fix (ListF a) -> GAContext a (Fix (ListF a))
selectAndCross pop = do 
    cfg <- ask
    let numToProduce = round $ (crossoverRate cfg) * 100
    let numToCrossover = (popSize cfg) - numToProduce
    let selectedParents = takeF numToCrossover $ sortF pop

    children <- reproduceFrom selectedParents numToProduce 
    return $ selectedParents +++ children

insertHeap :: Ord a => HOF a -> Fix (ListF a) -> HOF a
insertHeap hof = cata insert where
    insert Nil = hof
    insert (Cons a heap) = Heap.insert a heap

updateHOF :: Ord a => GASnapshot a -> Fix (ListF a) -> GASnapshot a
updateHOF res pop = res { hof = newHOF } where
    currentHOF = hof res
    newHOF = case (Heap.isEmpty currentHOF) of
        -- initialize the HOF with the `hofSize` best individuals
        True -> insertHeap currentHOF $ takeF (hofSize res) pop
        -- update the HOF with the best performers seen thus far
        False -> Heap.drop (lengthF pop) $ insertHeap currentHOF pop

bmap :: (a -> b) -> (Fix (ListF a)) -> (Fix (ListF b))
bmap f = cata ff where
    ff Nil = nil
    ff (Cons x m) = cons (f x) m

step :: Ord a => GASnapshot a -> GAContext a (GASnapshot a)
step snapshot = do
    cfg <- ask
    let pop = lastGeneration snapshot
    let score = fitness cfg
    -- select parents and create the next generation from them
    crossed <- selectAndCross pop
    -- mutate the next generation
    mutated <- mutatePop (mutationRateInd cfg) crossed
    -- update the HOF
    let newCtx = updateHOF snapshot mutated
    -- show intermediate results
    let best = (T.pack . show . reverse . map score . Heap.toList $ hof newCtx) :: T.Text
    let msg = T.concat ["best: ", best]
    tell $ ([msg] :: [T.Text])
    -- return the mutated generation
    return $ newCtx {lastGeneration = mutated}

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
    initialPop <- makePopulation (popSize cfg)

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
