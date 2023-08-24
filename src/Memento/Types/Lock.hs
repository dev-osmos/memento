{-# LANGUAGE TemplateHaskell #-}

module Memento.Types.Lock where

import Chronos (Datetime)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Local (CompositeTags (CompositeTags))
import Data.Default.Class (Default)
import Data.Default.Instances.Containers ()
import Data.Iso (Iso)
import Lens.Micro.Local (makeLenses)
import Memento.Cli (Action)
import Memento.Types.Common (SubjectId)
import Memento.Types.Static (StaticLock)

data LockFile = LockFile
  { locks :: Map SubjectId SubjectLock
  , log :: [LogEntry]
  }
  deriving stock (Generic)
  deriving anyclass (Default, FromJSON, ToJSON)

newtype SubjectLock
  = Static StaticLock
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via (CompositeTags SubjectLock)

data LogEntry = LogEntry
  { action :: Action
  , datetime :: Iso 8601 Datetime
  , actor :: Maybe Text
  , comment :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeLenses ''LockFile
