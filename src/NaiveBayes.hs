{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module NaiveBayes where

import           Control.Applicative
import qualified Control.Foldl as L
import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Proxy
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V
import           Frames
import           Frames.CSV
import           Frames.InCore
import           Frames.Rec
import           Linear
import qualified Pipes as P
import           Pipes hiding (Proxy)
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

data ClassDistribution =
  ClassDistribution {classProp :: Double
                    ,distrs :: [Distribution]} deriving (Show,Eq,Ord)


mean' :: (Ord c', CanDelete c rs, rs' ~ RDelete c rs,AllAre Double (UnColumn rs'),AsVinyl rs',c ~ (s:->c'))
      => Proxy c -> Frame (Record rs) -> M.Map c' [Double]
mean' p f =
  M.map (\(x,c) -> fmap (/ fromIntegral c) x) $ L.fold (sumFold p) f

sumFold :: (Ord c', CanDelete c rs, rs' ~ RDelete c rs, AllAre Double (UnColumn rs'),AsVinyl rs',c ~ (s:->c'))
          => Proxy c  -> L.Fold (Record rs) (M.Map c' ([Double],Int))
sumFold p =
  L.Fold (\m args ->
            M.insertWith
              (\(x,c) (x',c') ->
                 (x ^+^ x',c + c'))
              (getSingle p args)
              (recToList (rdel p args),1)
              m)
         M.empty
         id

var' :: (Ord c',CanDelete c rs,rs' ~ RDelete c rs,AllAre Double (UnColumn rs'),AsVinyl rs',c ~ (s :-> c'))
     => Proxy c -> Frame (Record rs) -> M.Map c' [Double]
var' p f = M.map (\(x,c) -> fmap (/ fromIntegral c) x) $ L.fold (varFold (mean' p f) p) f

varFold :: (Ord c',CanDelete c rs,rs' ~ RDelete c rs,AllAre Double (UnColumn rs'),AsVinyl rs',c ~ (s :-> c'))
        => (M.Map c' [Double]) -> Proxy c -> L.Fold (Record rs) (M.Map c' ([Double],Int))
varFold mean p =
  L.Fold (\m args -> let k = getSingle p args in
            M.insertWith
              (\(x,c) (x',c') ->
                 (x ^+^ x',c + c'))
              k
              (fmap (** 2) $
               recToList (rdel p args) ^-^
               (mean M.! k)
              ,1)
              m)
         M.empty
         id

classProps :: Ord c => Frame (Record '[s:->c]) -> M.Map c Double
classProps f = M.map (/ fromIntegral (frameLength f)) $ L.fold classPropFold f

classPropFold :: Ord c => L.Fold (Record '[s:->c]) (M.Map c Double)
classPropFold = L.Fold (\m args -> M.insertWith (+) (single args) 1 m) M.empty id

distributions' :: (Ord c',CanDelete c rs,rs' ~ RDelete c rs,AllAre Double (UnColumn rs'),AsVinyl rs',c ~ (s :-> c'))
               => Proxy c -> Frame (Record rs) -> M.Map c' ClassDistribution
distributions' p f =
  M.intersectionWith
    ClassDistribution
    (classProps (fmap (select (proxySing p)) f)) $
  M.intersectionWith (zipWith Distribution)
                     (var' p f)
                     (mean' p f)

dumb2 :: [a] -> (a,a,a)
dumb2 [a,b,c] = (a,b,c)

filter' :: RecVec rs => (Record rs -> Bool) -> FrameRec rs -> IO (FrameRec rs)
filter' p f = inCoreAoS $ (P.each f) >-> P.filter p

virginica :: Frame Iris -> IO (Frame Iris)
virginica = filter' ((==Virginica) . view species)

versicolor :: Frame Iris -> IO (Frame Iris)
versicolor = filter' ((==Versicolor) . view species)

setosa :: Frame Iris -> IO (Frame Iris)
setosa = filter' ((==Setosa) . view species)

getDistributions :: Frame Iris -> M.Map Class ClassDistribution
getDistributions f = distributions' [pr|Species|] f

gaussian :: Distribution -> Double -> Double
gaussian (Distribution{..}) x =
  1 /
  sqrt (2 * pi * variance) *
  exp (-(x - mean) **
       2 /
       (2 * variance))

classConditional :: ClassDistribution -> Parameters -> Double
classConditional (ClassDistribution _ distrs) p =
  product $ zipWith gaussian distrs (recToList p)

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
     let distrs = getDistributions iris
     putStrLn (formatTable $
               predict' iris (distrs M.! Virginica, distrs M.! Versicolor, distrs M.! Setosa))
