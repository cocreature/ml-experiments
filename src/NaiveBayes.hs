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

import qualified Control.Foldl as L
import           Control.Lens
import qualified Data.Foldable as F
import           Data.List
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

data Distribution = Distribution { variance :: Double, mean :: Double } deriving (Show,Eq,Ord)

data ClassDistribution =
  ClassDistribution {classProp :: Double
                    ,distrs :: [Distribution]} deriving (Show,Eq,Ord)

type ConvertibleTo rs t = (AllAre t (UnColumn rs),AsVinyl rs)
mean' :: (Ord c', CanDelete c rs, rs' ~ RDelete c rs,ConvertibleTo rs' Double,c ~ (s:->c'))
      => Proxy c -> Frame (Record rs) -> M.Map c' [Double]
mean' p f =
  M.map (\(x,c) -> fmap (/ fromIntegral c) x) $ L.fold (sumFold p) f

sumFold :: (Ord c', CanDelete c rs, rs' ~ RDelete c rs, ConvertibleTo rs' Double,c ~ (s:->c'))
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

var' :: (Ord c',CanDelete c rs,rs' ~ RDelete c rs,ConvertibleTo rs' Double,c ~ (s :-> c'))
     => Proxy c -> Frame (Record rs) -> M.Map c' [Double]
var' p f = M.map (\(x,c) -> fmap (/ fromIntegral c) x) $ L.fold (varFold (mean' p f) p) f

varFold :: (Ord c',CanDelete c rs,rs' ~ RDelete c rs,ConvertibleTo rs' Double,c ~ (s :-> c'))
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

distributions' :: (Ord c',CanDelete c rs,rs' ~ RDelete c rs,ConvertibleTo rs' Double,c ~ (s :-> c'))
               => Proxy c -> Frame (Record rs) -> M.Map c' ClassDistribution
distributions' p f =
  M.intersectionWith
    ClassDistribution
    (classProps (fmap (select (proxySing p)) f)) $
  M.intersectionWith (zipWith Distribution)
                     (var' p f)
                     (mean' p f)

gaussian :: Distribution -> Double -> Double
gaussian (Distribution{..}) x =
  1 /
  sqrt (2 * pi * variance) *
  exp (-(x - mean) **
       2 /
       (2 * variance))

classConditional :: (ConvertibleTo rs Double) => ClassDistribution -> Record rs -> Double
classConditional (ClassDistribution _ distrs) p =
  product $ zipWith gaussian distrs (recToList p)

predict :: (ConvertibleTo rs Double) => M.Map c ClassDistribution -> Record rs -> c
predict m p = fst $ maxValue $ M.mapWithKey (\k a -> classConditional a p * classProp a) m


predict' :: (CanDelete c rs,rs' ~ RDelete c rs,ConvertibleTo rs' Double,c ~ (s :-> c'))
         => Proxy c
         -> Frame (Record rs)
         -> M.Map c' ClassDistribution
         -> [(c',c')]
predict' p f m =
  zip (F.toList $
       fmap (predict m) $
       fmap (rdel p) f)
      (F.toList $ fmap (getSingle p) f)
