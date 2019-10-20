module BinaryIndRec
    (
        BinaryIndRec (..),
        BinaryIndRec.mutate,
        BinaryIndRec.crossover,
        new,
        score
    ) where

import GA
import Recursive
import Data.Functor.Foldable (Fix (..), ListF (..), cata)
import Data.Functor.Foldable.Exotic (cataM, anaM)
import Control.Monad.RWS.Lazy (ask)

data BinaryIndRec = BIR (Fix (ListF Bool))

instance Eq BinaryIndRec where
    (BIR a) == (BIR b) = a == b

instance Ord BinaryIndRec where
    a `compare` b = (score a) `compare` (score b)

mutate :: Double -> BinaryIndRec -> GAContext BinaryIndRec BinaryIndRec
mutate p (BIR ind) = return . BIR =<< newInd where
    newInd = do
        indp <- randomD
        case indp < p of
            True -> cataM mutateM ind
            False -> return ind
    mutateM :: AlgebraM (GAContext BinaryIndRec) (ListF Bool) (Fix (ListF Bool))
    mutateM Nil = return $ nil
    mutateM (Cons bool c) = do
        cfg <- ask
        mutated <- mutateBool (mutationRateChr cfg) bool
        return $ cons mutated c

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

-- goal: do a zipWith choose on a and b into a Fix (ListF Bool)
crossover :: BinaryIndRec -> BinaryIndRec -> GAContext BinaryIndRec BinaryIndRec
crossover (BIR p1) (BIR p2) = return . BIR =<< anaM f (p1,p2) where
    f :: CoAlgebraM (GAContext BinaryIndRec) (ListF Bool) (Fix (ListF Bool), Fix (ListF Bool))
    f (Fix Nil,_) = return Nil
    f (_, Fix Nil) = return Nil
    f (Fix (Cons b1 i1), Fix (Cons b2 i2)) = do
        takeFirst <- randomBool
        let b = if takeFirst then b1 else b2
        return $ Cons b (i1, i2)