module Data.These where

data These a b
  = This a
  | That b
  | These a b
  deriving stock (Show)

instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
  This a <> This a' = This (a <> a')
  This a <> That b = These a b
  This a <> These a' b = These (a <> a') b
  That b <> This a = These a b
  That b <> That b' = That (b <> b')
  That b <> These a b' = These a (b <> b')
  These a b <> This a' = These (a <> a') b
  These a b <> That b' = These a (b <> b')
  These a b <> These a' b' = These (a <> a') (b <> b')
