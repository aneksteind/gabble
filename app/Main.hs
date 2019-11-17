{-# LANGUAGE NamedFieldPuns #-}

module Main where

import GA (evalGA, logHOF, GAConfig(..))
import qualified BinaryInd as BI
import qualified BinaryIndRec as BIR

import qualified Data.Text as T

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

