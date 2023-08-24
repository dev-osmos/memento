module Aws.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Show (ShowTextUnpack (ShowTextUnpack))

newtype AvailabilityZone = AvailabilityZone Text
  deriving newtype (IsString, FromJSON, ToJSON)
  deriving (Show) via ShowTextUnpack

newtype RdsInstanceId = RdsInstanceId Text
  deriving newtype (IsString, FromJSON, ToJSON)
  deriving (Show) via ShowTextUnpack

newtype RdsSnapshotId = RdsSnapshotId Text
  deriving newtype (IsString, FromJSON, ToJSON)
  deriving (Show) via ShowTextUnpack
