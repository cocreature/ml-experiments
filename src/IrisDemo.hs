{-# LANGUAGE TemplateHaskell #-}
module IrisDemo where

import Frames
import Frames.CSV
import Pipes
import Helpers
import NaiveBayes
import Types


tableTypes' rowGen {columnUniverse = $(colQ ''MyColumns), rowTypeName = "Iris" } "iris.dat"

irisStream :: Producer Iris IO ()
irisStream = readTableOpt irisParser "iris.dat"

loadIris :: IO (Frame Iris)
loadIris = inCoreAoS irisStream

runNaiveBayes :: IO ()
runNaiveBayes =
  do iris <- loadIris
     let distrs = distributions [pr|Species|] iris
     putStrLn (formatTable $
               predict' [pr|Species|] iris distrs)
