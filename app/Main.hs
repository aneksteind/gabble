module Main where

import GA (evalGA, GAConfig(..), GASnapshot(hof), logHOF)
import qualified BinaryInd as BI
import qualified BinaryIndRec as BIR
import BinaryIndRec (BinaryIndRec)
import BinaryInd (BinaryInd)

import qualified Data.Heap as Heap
import qualified Data.Text as T

main :: IO ()
main = do

    let cfg = Config {
        mutationRateInd = 0.8
      , mutationRateChr = 0.02
      , crossoverRate = 0.8
      , popSize = 100
      , mutate = BIR.mutate
      , crossover = BIR.crossover
      , randomIndividual = BIR.new
      , selectionMethod = BIR.select
      , fitness = BIR.score
      , numGenerations = 200
      , hofSize = 3
      , logFunc = logHOF
    }

    -- run the genetic algorithm
    (finalCtx, progress) <- evalGA cfg

    -- output the average and best results as they're found
    mapM_ (putStrLn . T.unpack) progress

    putStr "Final best scores: "
    putStrLn . show . map (fitness cfg) . reverse . Heap.toList . hof $ finalCtx

