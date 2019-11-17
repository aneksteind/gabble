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
    -- the probability an individual is mutated
    mutationRateInd :: Double 
    -- the probability a chromosome of an individual is mutated
  , mutationRateGene :: Double 
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
```

The most difficult part about the interface is defining the mutation, crossover, and random-individual methods. These must return a `GAContext i i` where `i` is the user's representation of an individual.

`GAContext` is a newtype for the `RWS` monad:

```haskell
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
```

which allows the user to utilize the `PureMT` pseudo-random number generator, write intermediate logging data, and reference the configurations they pass into the genetic algorithm.

An example of the above can be found in `BinaryInd.hs`:

```haskell
data BinaryInd = BI [Bool] deriving (Show)

instance Ord BinaryInd where
    b1 `compare` b2 = (score b1) `compare` (score b2)

instance Eq BinaryInd where
    (BI b1) == (BI b2) = b1 == b2


-- mutate a binary string representation
mutate :: BinaryInd -> GAContext BinaryInd BinaryInd
mutate ind@(BI bs) = do
        -- grab individual and chromosome mutation rates
        Config{mutationRateGene, mutationRateInd} <- ask
        -- get a random double
        indp <- randomD
        -- if the value is less than mutation rate for an individual
        if indp < mutationRateInd then
            -- mutate each bit with `mutationRateGene` probability
            fmap BI $ mapM (mutateBool mutationRateGene) bs
        else
            -- return the unaltered individual
            return ind

-- recombine two individuals from the population
crossover :: BinaryInd -> BinaryInd -> GAContext BinaryInd BinaryInd
crossover (BI i1) (BI i2) = do
        -- get the crossover rate
        Config{crossoverRate} <- ask
        -- get a random double
        indp <- randomD
        if indp < crossoverRate then do -- perform crossover
            -- get booleans specifying which gene to take
            code <- replicateM (length i1) randomBool
            -- choose genetic material from first or second parent
            let eitherOr = (\takeThis this that -> if takeThis then this else that)
            -- perform uniform crossover
            return . BI $ zipWith3 eitherOr code i1 i2
        else do
            -- choose the genetic material from one of the parents
            chooseFirstParent <- randomBool
            return . BI $ if chooseFirstParent then i1 else i2

-- create an individual, represented by a list, by
-- initializing its elements randomly
new :: GAContext BinaryInd BinaryInd
new = fmap BI $ replicateM 500 randomBool

-- count the number of `True` bools in the chromosome
score :: BinaryInd -> Double
score (BI bs) = fromIntegral . length . filter id $ bs

select :: Ord a => Vector a -> GAContext a (Vector a)
select pop = do
    -- get the population size
    Config{popSize} <- ask
    -- get the number of individuals to breed
    let numToSelect = round $ 0.2 * (fromIntegral popSize)
    -- get the top 20% of the best-performing individuals
    let selectedParents = V.take numToSelect . V.reverse $ V.modify sort pop
    return selectedParents
```

Once `mutate`, `crossover`, `new`, and `fitness` have been defined, we can optimize for fitness. The GA will take care of initializing the population and evolving that population for a specified number of generations.

```haskell
main :: IO ()
main = do

    let cfg = Config {
        mutationRateInd = 0.8
      , mutationRateGene = 0.02
      , crossoverRate = 0.7
      , popSize = 100
      , mutate = BI.mutate
      , crossover = BI.crossover
      , randomIndividual = BI.new
      , selectionMethod = BI.select
      , fitness = BI.score
      , numGenerations = 200
      , hofSize = 1
      , logFunc = logHOF
    }

    -- run the genetic algorithm
    (finalSnapshot, progress) <- evalGA cfg

    -- output the best fitnesses as they're found
    mapM_ (putStrLn . T.unpack) progress
```

## Contributing

Pull requests for the following will be considered:

- bug fixes
- performance improvements
- examples