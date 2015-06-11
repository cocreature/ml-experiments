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
import qualified Data.Map.Lazy as ML
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

distributions :: (Ord c',CanDelete c rs,rs' ~ RDelete c rs,ConvertibleTo rs' Double,c ~ (s :-> c'))
                     => Proxy c -> Frame (Record rs) -> M.Map c' ClassDistribution
distributions p f =
  let n = fromIntegral $ frameLength f
      m' =
        fmap (\(a,b,c) -> (fmap (/c) a, fmap (/c) b,c)) $ L.fold (L.Fold (\m args ->
                          let k =
                                getSingle p args
                          in M.insertWith
                               (\(x,y,c) (x',y',c') ->
                                  (x ^+^ x',y ^+^ y',c + c'))
                               k
                               (fmap (** 2) $ -- variance
                                recToList (rdel p args) ^-^
                                view _2 (m' M.! k) -- get mean by tying the knot
                               ,recToList (rdel p args) -- used for mean
                               ,1) -- counter for each class
                               m)
                       M.empty
                       id)
               f
  in M.map (\(var,mean,c) -> ClassDistribution (c / n) (zipWith Distribution var mean)) $ m'


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

predictR :: (ConvertibleTo rs Double) => M.Map c ClassDistribution -> Record rs -> c
predictR m p = fst $ maxValue $ M.mapWithKey (\k a -> classConditional a p * classProp a) m

predict :: (CanDelete c rs,rs' ~ RDelete c rs,ConvertibleTo rs' Double,c ~ (s :-> c'))
        => Proxy c
        -> Frame (Record rs)
        -> M.Map c' ClassDistribution
        -> [(c',c')]
predict p f m =
  zip (F.toList $
       fmap (predictR m) $
       fmap (rdel p) f)
      (F.toList $ fmap (getSingle p) f)
