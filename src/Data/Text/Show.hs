module Data.Text.Show where

import Data.Text qualified as Text (unpack)
import Text.Show (show)

newtype ShowTextUnpack = ShowTextUnpack Text

instance Show ShowTextUnpack where
  show (ShowTextUnpack t) = Text.unpack t
