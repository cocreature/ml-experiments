{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Helpers where

import           Control.Applicative hiding (empty)
import           Control.Monad
import           Data.Function
import           Data.List.Extra
import           Data.Map as M hiding (map,foldr)
import           Data.Maybe
import           Data.Proxy
import           Data.Vinyl
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V
import           Frames
import           Frames.RecF
import           Frames.TypeLevel
import           Text.PrettyPrint.Boxes


formatTable :: (Enum a, Bounded a, Show a, Ord a) => [(a,a)] -> String
formatTable xs =
  let values = [minBound .. maxBound]
      counts' =
        foldr (\x m -> insertWith (+) x 1 m) empty xs
      counts =
        chunksOf (length values) $
        map (text . show) $
        map (\x ->
               fromMaybe 0 $
               M.lookup x counts')
            (liftM2 (,) values values)
      classNames =
        map (text . show) $
        values
  in render $
     hsep 1
          bottom
          (map (vsep 1 right)
               (zipWith (:) (nullBox : classNames) $
                transpose $
                zipWith (:) (classNames) counts))

combineRecordV :: forall c f ts. (Applicative f, LAll c ts) => Proxy c -> (forall a. c a => a -> a -> a) -> V.Rec f ts -> V.Rec f ts -> V.Rec f ts
combineRecordV _ f = go
  where go :: LAll c ts' => V.Rec f ts' -> V.Rec f ts' -> V.Rec f ts'
        go V.RNil V.RNil = V.RNil
        go (x V.:& xs) (y V.:& ys) = liftA2 f x y V.:& go xs ys

combineRecord :: forall f c ts. (Applicative f,LAll c (UnColumn ts),AsVinyl ts) => Proxy c -> (forall a. c a => a -> a -> a) -> Rec f ts -> Rec f ts -> Rec f ts
combineRecord p f f1 f2 = fromVinyl $ combineRecordV p f (toVinyl f1) (toVinyl f2)

proxySing :: Proxy c -> Proxy '[c]
proxySing Proxy = Proxy

single :: Record '[s:->x] -> x
single = singleV . toVinyl

singleV :: V.Rec V.Identity '[x] -> x
singleV (V.Identity x V.:& V.RNil) = x

getSingle :: RElem (s :-> x) rs (V.RIndex (s :-> x) rs)
          => Proxy (s :-> x) -> Record rs -> x
getSingle p args = single $ select (proxySing p) args

maxValue :: Ord a => M.Map k a -> (k,a)
maxValue = head . sortBy (flip (on compare snd)) . M.toList
