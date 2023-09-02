module Data.These where

data These a b
  = This a
  | That b
  | These a b
  deriving stock (Show)
