module Memento.Types.Built where

import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)
import Memento.Types.Static (StaticId)

newtype BuiltDoc = BuiltDoc
  { locks :: Map StaticId (Vector BuiltLock)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype BuiltLock = BuiltLock {built :: FilePath}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
