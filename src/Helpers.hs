module Helpers where

import Data.Map as M hiding (map,foldr)
import Control.Monad
import Data.List.Extra
import Text.PrettyPrint.Boxes
import Data.Maybe


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
