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


mutate :: Double -> BinaryInd -> GAContext BinaryInd BinaryInd
mutate p ind@(BI bs) = do
        Config{mutationRateChr = rate} <- ask
        indp <- randomD
        if indp < p then
            fmap BI $ mapM (mutateBool rate) bs
        else 
            return ind

crossover :: BinaryInd -> BinaryInd -> GAContext BinaryInd BinaryInd
crossover (BI i1) (BI i2) = do
        -- get booleans specifying which gene to take
        code <- replicateM (length i1) randomBool
        let eitherOr = (\takeThis this that -> if takeThis then this else that)
        return . BI $ zipWith3 eitherOr code i1 i2

-- create an individual, represented by a list, by
-- initializing its elements randomly;
-- each element must be mutatable
new :: GAContext BinaryInd BinaryInd
new = fmap BI $ replicateM 500 randomBool

-- count the number of `True` bools in the chromosome
score :: BinaryInd -> Double
score (BI bs) = fromIntegral . length . filter id $ bs

select :: Ord a => Vector a -> GAContext a (Vector a)
select pop = do
    Config{crossoverRate, popSize} <- ask

    let numToSelect = round $ (1.0 - crossoverRate) * (fromIntegral popSize)
    let selectedParents = V.take numToSelect . V.reverse $ V.modify sort pop

    return selectedParents

    