module Memento.Types.Static where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Local (CompositeTags (CompositeTags))
import Data.Default.Instances.Containers ()
import Data.Tree (Tree (..))
import Data.TreeJson (TreeJson (TreeJson))
import Data.TreePath (TreePath)
import Memento.Types.Dynamic (DynamicId, DynamicVersion)

newtype StaticVersion = GitVersion {rev :: Text}
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (CompositeTags StaticVersion)

data StaticConfig = StaticConfig
  { source :: StaticSource
  , dynamics :: [DynamicId]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StaticLock = StaticLock
  { history :: Tree StaticNodeLock
  , current :: TreePath
  }
  deriving stock (Show)

instance FromJSON StaticLock where
  parseJSON = do
    parseJSON >>> fmap \StaticLockJson {history = TreeJson history, current} -> StaticLock {history, current}

instance ToJSON StaticLock where
  toJSON StaticLock {history, current} = toJSON StaticLockJson {history = TreeJson history, current}

data StaticLockJson = StaticLockJson
  { history :: TreeJson StaticNodeLock
  , current :: TreePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data StaticSource = Git {url :: Text, ref :: Text}
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (CompositeTags StaticSource)

data StaticNodeLock = StaticNodeLock
  { version :: StaticVersion
  , snapshots :: Map DynamicId DynamicVersion
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
