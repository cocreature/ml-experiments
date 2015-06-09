{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module NaiveBayes where

import qualified Control.Foldl as L
import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Vinyl as V
import           Frames
import           Frames.CSV
import           Frames.InCore
import           Frames.Rec
import           Linear
import           Pipes
import qualified Pipes as P
import qualified Pipes.Prelude as P

import           Helpers
import           Types

tableTypes' rowGen {columnUniverse = $(colQ ''MyColumns), rowTypeName = "Iris" } "iris.dat"

irisStream :: Producer Iris IO ()
irisStream = readTableOpt irisParser "iris.dat"

loadIris :: IO (Frame Iris)
loadIris = inCoreAoS irisStream

type Parameters = Record '[SepalLength,SepalWidth,PetalLength,PetalWidth]

parameters :: Frame Iris -> Frame Parameters
parameters = fmap (rdel [pr|Species|])

classVals :: Frame Iris -> Frame Class
classVals = fmap (view species)

data Distribution = Distribution { variance :: Double, mean :: Double } deriving (Show,Eq,Ord)

parameterMean :: Lens' Parameters Double -> Frame Parameters -> Double
parameterMean l f =
  L.fold L.sum (fmap (view l) f) /
  (fromIntegral $
   L.fold L.length f)

parameterVar :: Lens' Parameters Double -> Frame Parameters -> Double
parameterVar l f =
  let m = parameterMean l f
  in L.fold L.sum
            (fmap (\r ->
                     (view l r - m) ^ 2) f) /
               (fromIntegral $
                L.fold L.length f)

parameterDistribution :: Lens' Parameters Double -> Frame Parameters -> Distribution
parameterDistribution l f = let m = parameterMean l f
                                v = parameterVar l f
                            in
                            Distribution {mean = m, variance = v}

data ClassDistribution =
  ClassDistribution {classProp :: Double
                    ,distrs :: (Distribution,Distribution,Distribution,Distribution)} deriving (Show,Eq,Ord)


distributions :: Frame Parameters -> (Distribution,Distribution,Distribution,Distribution)
distributions f =
  (parameterDistribution sepalLength f
  ,parameterDistribution sepalWidth f
  ,parameterDistribution petalLength f
  ,parameterDistribution petalWidth f)

mean' :: (AllAre Double (UnColumn rs),AsVinyl rs)
      => Frame (Record rs) -> [Double]
mean' f = fmap (/ (fromIntegral $ frameLength f)) $ L.fold meanFold f

meanFold :: (AllAre Double (UnColumn rs),AsVinyl rs)
         => L.Fold (Record rs) [Double]
meanFold =
  L.Fold (\acc args -> recToList args ^+^ acc)
         zero
         id

var' :: (AllAre Double (UnColumn rs),AsVinyl rs)
     => Frame (Record rs) -> [Double]
var' f = fmap (/ (fromIntegral $ frameLength f)) $ L.fold (varFold (mean' f)) f

varFold :: (AllAre Double (UnColumn rs),AsVinyl rs) => [Double] -> L.Fold (Record rs) [Double]
varFold mean = L.Fold (\acc args -> fmap (**2) (recToList args ^-^ mean) ^+^ acc) zero id

distributions' :: (AllAre Double (UnColumn rs),AsVinyl rs) => Frame (Record rs) -> [Distribution]
distributions' f = zipWith Distribution (var' f) (mean' f)

filter' :: RecVec rs => (Record rs -> Bool) -> FrameRec rs -> IO (FrameRec rs)
filter' p f = inCoreAoS $ (P.each f) >-> P.filter p

virginica :: Frame Iris -> IO (Frame Iris)
virginica = filter' ((==Virginica) . view species)

versicolor :: Frame Iris -> IO (Frame Iris)
versicolor = filter' ((==Versicolor) . view species)

setosa :: Frame Iris -> IO (Frame Iris)
setosa = filter' ((==Setosa) . view species)

getDistributions :: Frame Iris -> IO (ClassDistribution,ClassDistribution,ClassDistribution)
getDistributions f =
  do virginica' <- virginica f
     versicolor' <- versicolor f
     setosa' <- setosa f
     let virginicaDistr =
           distributions (parameters virginica')
         versicolorDistr =
           distributions (parameters versicolor')
         setosaDistr =
           distributions (parameters setosa')
     return (ClassDistribution (classProp virginica')
                               virginicaDistr
            ,ClassDistribution (classProp versicolor')
                               versicolorDistr
            ,ClassDistribution (classProp setosa')
                               setosaDistr)
  where n = fromIntegral (frameLength f)
        classProp x = fromIntegral (frameLength x) / n

gaussian :: Distribution -> Double -> Double
gaussian (Distribution{..}) x =
  1 /
  sqrt (2 * pi * variance) *
  exp (-(x - mean) **
       2 /
       (2 * variance))

classConditional :: ClassDistribution -> Parameters -> Double
classConditional (ClassDistribution _ (a,b,c,d)) p =
  gaussian a (p ^. sepalLength) *
  gaussian b (p ^. sepalWidth) *
  gaussian c (p ^. petalLength) *
  gaussian d (p ^. petalWidth)

predict :: (ClassDistribution,ClassDistribution,ClassDistribution) -> Parameters -> Class
predict (a,b,c) p
  | pa >= pb && pa >= pc = Virginica
  | pb >= pa && pb >= pc = Versicolor
  | pc >= pa && pc >= pb = Setosa
  where pa = classConditional a p * (classProp a)
        pb = classConditional b p * (classProp b)
        pc = classConditional c p * (classProp c)

predict' :: Frame Iris -> (ClassDistribution,ClassDistribution,ClassDistribution) -> [(Class,Class)]
predict' f (a,b,c) =
  zip (F.toList $
       fmap (predict (a,b,c)) $
       parameters f)
      (F.toList $ classVals f)


runNaiveBayes :: IO ()
runNaiveBayes =
  do iris <- loadIris
     distrs <- getDistributions iris
     putStrLn (formatTable $
               predict' iris distrs)
