# gabble

A small genetic algorithm library meant for prototyping various individual representations and genetic operators.


## Building
`stack install`

## Running
`stack exec gabble-exe`

## Usage

Users can define their own individuals and mutation operators or utilize the (limited) built-in ones. The primary interface of the library is a `GAConfig` in which the user can specify a number of different parameters that dictate the execution of the GA.

```haskell
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
```

The most difficult part about the interface is defining the mutation, crossover, and random-individual methods. These must return a `GAContext i i` where `i` is the user's representation of an individual.

`GAContext` is a newtype for the `RWS` monad:

```haskell
newtype GAContext i a = GAContext {
    ctx :: RWS (GAConfig i) [T.Text] PureMT a
}    deriving (Functor, Applicative, Monad, MonadReader (GAConfig i), MonadWriter [T.Text])
```

which allows the user to utilize the `PureMT` pseudo-random number generator, write intermediate logging data, and reference the configurations they pass into the genetic algorithm.

An example of the above can be found in `BinaryInd.hs`:

```haskell
data BinaryInd = BI [Bool] deriving (Show)

instance Ord BinaryInd where
    b1 `compare` b2 = (fitness b1) `compare` (fitness b2)

instance Eq BinaryInd where
    (BI b1) == (BI b2) = b1 == b2


mutate :: Double -> BinaryInd -> GAContext BinaryInd BinaryInd
mutate p ind@(BI bs) = do
        cfg <- ask
        indp <- randomD
        case indp < p of
            True -> do
                mutated <- mapM (mutateBool $ mutationRateChr cfg) bs
                return $ BI mutated
            False -> do
                return ind

crossover :: BinaryInd -> BinaryInd -> GAContext BinaryInd BinaryInd
crossover (BI i1) (BI i2) = do
        -- get booleans specifying which gene to take
        code <- replicateM (length i1) randomBool
        let pairing = zip i1 i2
        return . BI $ zipWith (\withI1 (bi1, bi2) -> if withI1 then bi1 else bi2) code pairing

-- create an individual, represented by a list, by
-- initializing its elements randomly;
-- each element must be mutatable
new :: GAContext BinaryInd BinaryInd
new = do
    -- an individual is a list of 500 booleans
    rep <- replicateM 500 randomBool
    return $ BI rep

-- count the number of `True` bools in the chromosome
fitness :: BinaryInd -> Double
fitness (BI bs) = fromIntegral . length . filter id $ bs
```

Once `mutate`, `crossover`, `new`, and `fitness` have been defined, we can optimize for fitness. The GA will take care of initializing the population and evolving that population for a specified number of generations.

```haskell
module Main where

import GA
import qualified BinaryInd as BI
import BinaryInd (BinaryInd)

import qualified Data.Heap as Heap
import qualified Data.Text as T
import Control.Monad.RWS.Lazy (evalRWS)
import System.Random.Mersenne.Pure64 (pureMT)

main :: IO ()
main = do

    let cfg = Config {
        mutationRateInd = 0.8
      , mutationRateChr = 0.02
      , crossoverRate = 0.8
      , popSize = 100
      , mutate = BI.mutate
      , crossover = BI.crossover
      , randomIndividual = BI.new
      , selectionMethod = Tournament 2
      , fitness = BI.fitness
      , numGenerations = 200
    }

    -- run the genetic algorithm
    let (finalCtx, progress) = evalRWS (ctx runGA) cfg (pureMT 100) :: (GASnapshot BinaryInd, [T.Text])

    -- output the average and best results as they're found
    mapM_ (putStrLn . T.unpack) progress

    putStr "Final best scores: "
    putStrLn . show . map (fitness cfg) . reverse . Heap.toList . hof $ finalCtx
```