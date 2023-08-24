module Data.Iso where

import Chronos (Datetime, decodeUtf8BytesIso8601ZonelessSpaced, encodeIso8601)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), withText)
import Data.Bytes (fromByteString)
import Data.Text qualified as Text (replace, span)
import Text.Show (show)

-- | @Iso n a@ corresponds to ISO-n defined representation of @a@
newtype Iso (n :: Nat) a = Iso a

instance FromJSON (Iso 8601 Datetime) where
  parseJSON =
    withText "ISO-8601 datetime with space" $
      maybe (fail "decodeUtf8BytesIso8601ZonelessSpaced returned Nothing") (pure . Iso)
        . decodeUtf8BytesIso8601ZonelessSpaced
        . fromByteString
        . encodeUtf8

instance ToText (Iso 8601 Datetime) where
  toText = fst . Text.span (/= '.') . Text.replace "T" " " . encodeIso8601 . coerce

instance Show (Iso 8601 Datetime) where
  show = toString . toText

instance ToJSON (Iso 8601 Datetime) where
  toJSON = String . toText
