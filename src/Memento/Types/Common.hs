module Memento.Types.Common where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Show (ShowTextUnpack (ShowTextUnpack))

-- import Toml.FromValue (FromKey(fromKey))

newtype SubjectId = SubjectId Text
  deriving newtype (Eq, Ord, IsString, FromJSON, FromJSONKey, ToJSON, ToJSONKey)
  deriving (Show) via ShowTextUnpack

-- instance FromKey SubjectId where fromKey = coerce . fromKey
