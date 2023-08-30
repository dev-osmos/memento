{-# LANGUAGE TemplateHaskell #-}

module Memento.Types.Config where

import Control.Lens.Local (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Local (CompositeTags (CompositeTags))
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

makeLenses ''ConfigFile
makeLenses ''SubjectConfig
