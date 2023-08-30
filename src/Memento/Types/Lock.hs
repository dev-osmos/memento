{-# LANGUAGE TemplateHaskell #-}

module Memento.Types.Lock where

import Control.Lens.Local (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default)
import Data.Default.Instances.Containers ()
import Memento.Types.Static (StaticId, StaticLocks)

newtype LockFile = LockFile
  { locks :: Map StaticId StaticLocks
  -- , log :: [LogEntry]
  }
  deriving stock (Generic)
  deriving anyclass (Default, FromJSON, ToJSON)

-- newtype SubjectLocks
--   = Static { versions :: StaticLocks }
--   deriving stock (Generic, Show)
--   deriving (FromJSON, ToJSON) via (CompositeTags SubjectLocks)

-- data LogEntry = LogEntry
--   { action :: Action
--   , datetime :: Iso 8601 Datetime
--   , actor :: Maybe Text
--   , comment :: Maybe Text
--   }
--   deriving stock (Generic)
--   deriving anyclass (FromJSON, ToJSON)

makeLenses ''LockFile

-- makeLenses ''SubjectLocks
