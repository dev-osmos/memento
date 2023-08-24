module Memento.Types.Dynamic where

import Aws.Types (AvailabilityZone, RdsInstanceId, RdsSnapshotId (RdsSnapshotId))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.Local (CompositeTags (CompositeTags))
import Data.Default.Instances.Containers ()
import Memento.Types.Common (SubjectId)

newtype DynamicId = DynamicId SubjectId
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype DynamicVersion = AwsRdsSnapshot {snapshotId :: RdsSnapshotId}
  deriving stock (Generic)
  deriving newtype (Show)
  deriving (FromJSON, ToJSON) via (CompositeTags DynamicVersion)

data DynamicSource = AwsRdsSource {availabilityZone :: AvailabilityZone, dbInstance :: RdsInstanceId}
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via (CompositeTags DynamicSource)

newtype DynamicConfig = DynamicConfig
  { source :: DynamicSource
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
