module Memento.Cli where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Local (CompositeTags (CompositeTags))

data Cli = Cli
  { rootPath :: FilePath
  , comment :: Maybe Text
  , verbose :: Bool
  , action :: Action
  }
  deriving stock (Show)

data Action
  = -- | Update lock with information from config
    Lock
  | -- | List versions
    History
  | -- | Update a service
    Update
  | -- | Switch a service or state to another version
    Switch
  | -- | Print action log
    Log
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via (CompositeTags Action)

needsLogging :: Action -> Bool
needsLogging Log {} = False
needsLogging History {} = False
needsLogging _ = True
