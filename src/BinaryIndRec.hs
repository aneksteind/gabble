{-# LANGUAGE NamedFieldPuns #-}

module BinaryIndRec
    (
        BinaryIndRec (..),
        BinaryIndRec.mutate,
        BinaryIndRec.crossover,
        new,
        score,
        select
    ) where

import GA
import Recursive
import Data.Functor.Foldable (ListF (..), cata)
import Data.Functor.Foldable.Exotic (cataM, anaM)
import Control.Monad.RWS.Lazy (ask)
import Data.Vector (Vector(..))
import qualified Data.Vector as V
import Data.Vector.Algorithms.Merge (sort)

data BinaryIndRec = BIR [Bool]

instance Eq BinaryIndRec where
    (BIR a) == (BIR b) = a == b

instance Ord BinaryIndRec where
    a `compare` b = (score a) `compare` (score b)

mutate :: BinaryIndRec -> GAContext BinaryIndRec BinaryIndRec
mutate (BIR ind) = return . BIR =<< newInd where
    newInd = do
        Config{mutationRateInd} <- ask
        indp <- randomD
        if indp < mutationRateInd then cataM mutateM ind else return ind
    mutateM :: AlgebraM (GAContext BinaryIndRec) (ListF Bool) [Bool]
    mutateM Nil = return $ []
    mutateM (Cons bool c) = do
        Config{mutationRateChr} <- ask
        mutated <- mutateBool mutationRateChr bool
        return $ mutated:c

new :: GAContext BinaryIndRec BinaryIndRec
new = (return . BIR) =<< anaM randomBools 500 where
    -- constructs a (ListF Bool) of a specified length
    randomBools :: CoAlgebraM (GAContext BinaryIndRec) (ListF Bool) Int
    randomBools 0 = return Nil
    randomBools n = do
        b <- randomBool
        return $ Cons b (n-1)

score :: BinaryIndRec -> Double
score (BIR ind) = cata alg ind where
    alg :: Algebra (ListF Bool) Double
    alg Nil = 0.0
    alg (Cons i acc) = acc + if i then 1.0 else 0.0


crossover :: BinaryIndRec -> BinaryIndRec -> GAContext BinaryIndRec BinaryIndRec
crossover (BIR p1) (BIR p2) = return . BIR =<< anaM f (p1,p2) where
    f :: CoAlgebraM (GAContext BinaryIndRec) (ListF Bool) ([Bool], [Bool])
    f ([],_) = return Nil
    f (_, []) = return Nil
    f (b1:i1, b2:i2) = do
        takeFirst <- randomBool
        let b = if takeFirst then b1 else b2
        return $ Cons b (i1, i2)

select :: Ord a => Vector a -> GAContext a (Vector a)
select pop = do
    Config{popSize} <- ask
    -- get the number of individuals to breed
    let numToSelect = round $ 0.2 * (fromIntegral popSize)
    -- select the individuals
    let selectedParents = V.take numToSelect . V.reverse $ V.modify sort pop
    return selectedParents