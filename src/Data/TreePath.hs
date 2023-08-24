module Data.TreePath where

import Data.Aeson (FromJSON, ToJSON)

newtype TreePath = TreePath [Int]
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)
