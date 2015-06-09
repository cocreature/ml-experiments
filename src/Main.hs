{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs,
             OverloadedStrings, PatternSynonyms, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeOperators,
             ViewPatterns #-}
module Main where

import Helpers
import Data.Maybe
import Data.List.Extra hiding (lookup)
import Text.PrettyPrint.Boxes
import Control.Monad
import Data.Map.Strict as M hiding (foldr,map)
import Data.List hiding (lookup)
import Debug.Trace
import Control.Applicative hiding (empty)
import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Data.Proxy (Proxy(..))
import Control.Lens
import Frames
import Frames.InCore
import Frames.CSV (readTableOpt, rowGen, RowGen(..),colQ)
import Pipes hiding (Proxy,each)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Types

tableTypes' rowGen {columnUniverse = $(colQ ''MyColumns), rowTypeName = "Iris" } "iris.dat"

main :: IO ()
main = do iris <- loadIris
          distrs <- getDistributions iris
          putStrLn (formatTable $ predict' iris distrs)

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
