module Common (
    randomD,
    randomW,
    randomBool,
    mutateBool,
    Algebra,
    AlgebraM,
    CoAlgebraM
) where

import GA (GAContext (..))
import Recursive
import System.Random.Mersenne.Pure64 (randomDouble, randomWord, PureMT)
import Control.Monad.RWS.Lazy (rws)
import Data.Bits ((.&.))

random :: (PureMT -> (a, PureMT)) -> GAContext b a
random f = GAContext $ rws (\_ s -> let (a,s') = f s in (a, s', []))

randomD :: GAContext a Double
randomD = random randomDouble

randomW :: GAContext a Word
randomW = random randomWord

randomBool :: GAContext a Bool
randomBool = do
    x <- randomW
    return $ x .&. 1 /= 0

-- mutate a boolean by flipping it
mutateBool :: Double -> Bool -> GAContext a Bool
mutateBool p x = do
    indp <- randomD
    if indp < p then return $ not x else return x