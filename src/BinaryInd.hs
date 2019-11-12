{-# LANGUAGE NamedFieldPuns #-}

module BinaryInd
    (
        BinaryInd (..),
        BinaryInd.mutate,
        BinaryInd.crossover,
        new,
        score,
        select
    ) where


import GA
import Control.Monad (replicateM)
import Control.Monad.RWS.Lazy (ask)
import qualified Data.Vector as V
import Data.Vector (Vector(..), modify, toList, fromList)
import Data.Vector.Algorithms.Merge (sort)

data BinaryInd = BI [Bool] deriving (Show)

instance Ord BinaryInd where
    b1 `compare` b2 = (score b1) `compare` (score b2)

instance Eq BinaryInd where
    (BI b1) == (BI b2) = b1 == b2


-- mutate a binary string representation
mutate :: BinaryInd -> GAContext BinaryInd BinaryInd
mutate ind@(BI bs) = do
        -- grab individual and chromosome mutation rates
        Config{mutationRateChr, mutationRateInd} <- ask
        -- get a random double
        indp <- randomD
        -- if the value is less than mutation rate for an individual
        if indp < mutationRateInd then
            -- mutate each bit with `mutationRateChr` probability
            fmap BI $ mapM (mutateBool mutationRateChr) bs
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

    