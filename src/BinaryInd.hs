module BinaryInd
    (
        BinaryInd (..),
        BinaryInd.mutate,
        BinaryInd.crossover,
        new,
        score
    ) where


import GA
import Control.Monad (replicateM)
import Control.Monad.RWS.Lazy (ask)

data BinaryInd = BI [Bool] deriving (Show)

instance Ord BinaryInd where
    b1 `compare` b2 = (score b1) `compare` (score b2)

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
score :: BinaryInd -> Double
score (BI bs) = fromIntegral . length . filter id $ bs

    