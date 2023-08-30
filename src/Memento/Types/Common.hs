module Memento.Types.Common where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Show (ShowTextUnpack (ShowTextUnpack))

newtype SubjectId = SubjectId Text
  deriving newtype (Eq, Ord, IsString, FromJSON, FromJSONKey, ToJSON, ToJSONKey)
  deriving (Show) via ShowTextUnpack
