module Memento.Types.Config where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Local (CompositeTags (CompositeTags))
import Data.Map.Strict qualified as Map (singleton)
import Memento.Types.Common (SubjectId)
import Memento.Types.Dynamic (DynamicConfig)
import Memento.Types.Static (StaticConfig)

newtype ConfigFile = ConfigFile
  { subjects :: Map SubjectId SubjectConfig
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SubjectConfig
  = Static StaticConfig
  | Dynamic DynamicConfig
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via (CompositeTags SubjectConfig)

(~>) :: k -> v -> Map k v
(~>) = Map.singleton

infix 5 ~>
