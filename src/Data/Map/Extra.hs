{-# LANGUAGE FieldSelectors #-}

module Data.Map.Extra where

import Data.Map.Strict (intersectionWith, (\\))
import Data.These (These (..))

zip :: (Ord k) => Map k a -> Map k b -> Map k (These a b)
zip xs ys =
  mconcat
    [ This <$> (xs \\ ys)
    , intersectionWith These xs ys
    , That <$> (ys \\ xs)
    ]
