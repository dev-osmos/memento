{-# LANGUAGE TemplateHaskell #-}

module Memento.Types.Config where

import Control.Lens.Local (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Local (CompositeTags (CompositeTags))
import Data.Default.Class (Default)
import Memento.Types.Common (SubjectId)
import Memento.Types.Dynamic (DynamicConfig)
import Memento.Types.Static (StaticConfig)

newtype ConfigDoc = ConfigDoc
  { subjects :: Map SubjectId SubjectConfig
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromJSON, ToJSON)

data SubjectConfig
  = Static StaticConfig
  | Dynamic DynamicConfig
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via (CompositeTags SubjectConfig)

makeLenses ''ConfigDoc
makeLenses ''SubjectConfig
