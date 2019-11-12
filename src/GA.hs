{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

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

import Recursive (CoAlgebraM, AlgebraM)
import Control.Monad.RWS.Lazy (RWS, runRWS, evalRWS, ask, tell, get, put, MonadReader, MonadWriter, MonadState)
import Data.Functor.Foldable (ListF (..), cata, embed, project, Base(..), Recursive, Corecursive)
import Data.Fix (hyloM)
import Data.List(intersperse)
import Data.Bits ((.&.))
import qualified Data.Text as T
import qualified Data.Heap as Heap
import System.Random.Mersenne.Pure64 (randomInt, pureMT, PureMT, newPureMT, randomDouble, randomWord)
import qualified Data.Vector as V
import Data.Vector (Vector(..), (!))

-- the Hall Of Fame is min-heap of the best individuals
type HOF a = Heap.MinHeap a

type instance Base (Vector a) = ListF a

instance Recursive (Vector a) where
  project xs | V.null xs = Nil
             | otherwise = Cons (V.head xs) (V.tail xs)
  
instance Corecursive (Vector a) where
  embed (Cons x xs) = x `V.cons` xs
  embed Nil = V.empty

newtype GAContext indv a = GAContext {
    ctx :: RWS (GAConfig indv) [T.Text] PureMT a
} deriving (
        Functor, 
        Applicative, 
        Monad, 
        MonadReader (GAConfig indv), 
        MonadWriter [T.Text],
        MonadState PureMT
    )

data GAConfig i = Config {
    -- the probability an individual is mutated
    mutationRateInd :: Double 
    -- the probability a chromosome of an individual is mutated
  , mutationRateChr :: Double 
    -- the percentage of the population that gets replaced through recombination
  , crossoverRate :: Double 
    -- the population size
  , popSize :: Int 
    -- the mutation method
  , mutate :: i -> GAContext i i 
    -- the crossover method
  , crossover :: i -> i -> GAContext i i 
    -- the method to create a new individual
  , randomIndividual :: GAContext i i  
    -- the selection method
  , selectionMethod :: Vector i -> GAContext i (Vector i) 
    -- the fitness function (higher fitness is preferred)
  , fitness :: i -> Double 
    -- the number of generations
  , numGenerations :: Int 
    -- the `hofSize` best individuals across all generations
  , hofSize :: Int 
    -- function for information sourced from most recent snapshot
  , logFunc :: GASnapshot i -> GAContext i () 
}

data GASnapshot a = Snapshot {
    -- the collection of individuals from the last generation
    lastGeneration :: Vector a
    -- the collection of top performers, the Hall of Fame (HOF)
  , hof :: HOF a 
    -- the current generation id
  , generationNumber :: Int
} deriving (Show)

-- applies a random function within a context
random :: (PureMT -> (a, PureMT)) -> GAContext b a
random f = do
    s <- get
    let (a,s') = f s
    put s'
    return a

randomI :: GAContext a Int
randomI = random randomInt

randomD :: GAContext a Double
randomD = random randomDouble

randomW :: GAContext a Word
randomW = random randomWord

randomBool :: GAContext a Bool
randomBool = fmap (\x -> x .&. 1 /= 0) randomW

-- mutate a boolean by flipping it
mutateBool :: Double -> Bool -> GAContext a Bool
mutateBool p x = do
    indp <- randomD
    return $ if indp < p then not x else x

-- converts Fix (ListF a) into Vector a
toVector :: AlgebraM (GAContext a) (ListF a) (Vector a)
toVector = return . embed

-- creates a vector of random individuals
makePopulation :: Int -> GAContext a (Vector a)
makePopulation s = hyloM toVector addRandomInd s where
    -- creates a random individual and adds it to the collection
    addRandomInd :: CoAlgebraM (GAContext a) (ListF a) Int
    addRandomInd 0 = return Nil
    addRandomInd n = do
        -- get a new, random individual
        ind <- randomIndividual =<< ask
        -- add it to the collection
        return $ Cons ind (n-1)

-- repeatedly selects two new parents from `parents` from
-- which `n` total children are produced
crossAndMutate :: (Vector a) -> Int -> GAContext a (Vector a)
crossAndMutate parents n = hyloM toVector (newChild parents) n

-- selects two parents to breed, a child is born, joy to the world
newChild :: (Vector a) -> CoAlgebraM (GAContext a) (ListF a) Int
newChild parents 0 = return Nil
newChild parents m = do
    -- get mutation and crossover methods
    Config {crossover, mutate} <- ask
    -- get two random indices
    i <- randomI
    j <- randomI
    -- from the two indices, grab two parents
    let p1 = parents ! (i `mod` (length parents))
    let p2 = parents ! (j `mod` (length parents))
    -- make a child
    child <- crossover p1 p2
    -- mutate the child
    mutatedChild <- mutate child
    -- add the child to the collection
    return $ Cons mutatedChild (m-1)

-- inserts elements from a list into a heap
insertHeap :: Ord a => HOF a -> (Vector a) -> HOF a
insertHeap hof = cata insert where
    insert Nil = hof
    insert (Cons a heap) = Heap.insert a heap

-- updates the HOF by removing the worst-fit individuals from the min-heap
updateHOF :: Ord a => HOF a -> Vector a -> Int -> GAContext a (HOF a)
updateHOF hof pop hofSize = return . Heap.drop n $ oversizedHOF where
    -- insert all of the current population
    oversizedHOF = insertHeap hof pop
    -- drop all but hofSize individuals
    n = V.length pop - if Heap.isEmpty hof then hofSize else 0

logNothing :: b -> GAContext a ()
logNothing = const $ return ()

logHOF :: Ord a => GASnapshot a -> GAContext a ()
logHOF Snapshot{hof, generationNumber} = do
    -- get the fitness function
    Config {fitness} <- ask
    -- get string representations of the best individuals
    let best = map (T.pack . show . fitness) $ Heap.toList hof
    -- craft the comma-separated line
    let msg = [T.concat $ intersperse (T.pack ",") best]
    -- log the line
    tell msg

step :: Ord a => GASnapshot a -> GAContext a (GASnapshot a)
step (Snapshot lastGen hof genNumber) = do
    Config {hofSize, logFunc, popSize, selectionMethod} <- ask 
    -- select parents and create the next generation from them
    selectedParents <- selectionMethod lastGen
    -- use the set of parents to create and mutate a new generation
    children <- crossAndMutate selectedParents popSize
    -- update the HOF
    updatedHOF <- updateHOF hof children hofSize
    -- construct the new snapshot
    let nextSnapshot = Snapshot{
        lastGeneration = children,
        hof = updatedHOF,
        generationNumber = genNumber + 1
    }
    -- log intermediate results
    logFunc nextSnapshot
    -- return the mutated generation
    return nextSnapshot

-- a function reminiscent of iterateM that completes
-- after `n` evaluations, returning the `n`th result
runN :: Monad m => Int -> (a -> m a) -> a -> m a
runN 0 _ a = return a
runN n f a = do
    a' <- f a
    runN (n-1) f a'

runGA :: Ord a => GAContext a (GASnapshot a)
runGA = do
    Config {numGenerations, popSize, hofSize} <- ask
    -- initialize the population
    initialPop <- makePopulation popSize
    -- set up the initial Hall of Fame
    initialHOF <- updateHOF (Heap.empty :: HOF a) initialPop hofSize
    -- set up the initial snapshot
    let snapshot = Snapshot {
                lastGeneration = initialPop,
                hof = initialHOF,
                generationNumber = 0
              }
    -- run the genetic algorithm
    runN numGenerations step snapshot

-- from a new rng, run the genetic algorithm
evalGA :: Ord i => GAConfig i -> IO (GASnapshot i, [T.Text])
evalGA cfg = newPureMT >>= (return . evalGASeed cfg)

-- from a user-supplied rng, run the genetic algorithm
evalGASeed :: Ord i => GAConfig i -> PureMT -> (GASnapshot i, [T.Text])
evalGASeed cfg rng = evalRWS (ctx runGA) cfg rng

-- from a user-supplied rng, run the genetic algorithm and return the updated seed
runGASeed :: Ord i => GAConfig i -> PureMT -> (GASnapshot i, PureMT, [T.Text])
runGASeed cfg rng = runRWS (ctx runGA) cfg rng
