{-# LANGUAGE TemplateHaskell #-}

module Memento.Types.Static where

import Control.Lens.Local (makeLenses)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.Local (CompositeTags (CompositeTags))
import Data.Default.Instances.Containers ()
import Memento.Types.Common (SubjectId)
import Memento.Types.Dynamic (DynamicId, DynamicVersion)

newtype StaticId = StaticId SubjectId
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data StaticVersion = GitVersion {rev, sha256 :: Text}
  deriving stock (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via (CompositeTags StaticVersion)

data StaticConfig = StaticConfig
  { source :: StaticSource
  , dynamics :: [DynamicId]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Reverse-chronologically sorted (newest is first) locked versions
type StaticLocks = [StaticLock]

data StaticLock = StaticLock {original :: StaticSource, locked :: StaticVersion}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- instance FromJSON StaticLocks where
--   parseJSON = do
--     parseJSON >>> fmap \StaticLocksJson {history = TreeJson history} -> StaticLocks {history}

-- instance ToJSON StaticLocks where
--   toJSON StaticLocks {history} = toJSON StaticLocksJson {history = TreeJson history}

-- newtype StaticLocksJson = StaticLocksJson
--   { history :: TreeJson StaticNodeLock
--   -- , current :: TreePath
--   }
--   deriving stock (Generic)
--   deriving anyclass (FromJSON, ToJSON)

data StaticSource = Git {url, ref, attribute :: Text}
  deriving stock (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via (CompositeTags StaticSource)

data StaticNodeLock = StaticNodeLock
  { version :: StaticVersion
  , snapshots :: Map DynamicId DynamicVersion
  -- ^ Snapshots of dymanics taken when switching OFF this static version
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

makeLenses ''StaticLock
