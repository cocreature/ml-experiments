{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Frames
import Frames.InCore
import Control.Monad
import Frames.ColumnTypeable
import Data.Readable
import Data.Vector

data Class = Setosa | Versicolor | Virginica deriving (Show,Eq,Ord,Enum)

instance Readable Class where
  fromText "virginica" = return Virginica
  fromText "versicolor" = return Versicolor
  fromText "setosa" = return Setosa
  fromText _ = mzero

instance Parseable Class

type instance VectorFor Class = Vector

type MyColumns = Class ': CommonColumns
