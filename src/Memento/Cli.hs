module Memento.Cli where

import Memento.Types.Dynamic (DynamicId)
import Memento.Types.Static (StaticId)
import Options.Applicative (Parser, command, flag', help, idm, info, long, metavar, short, strArgument, strOption, subparser, switch, value)

data Cli = Cli
  { rootPath :: FilePath
  , -- , comment :: Maybe Text
    verbose :: Bool
  , action :: Action
  }
  deriving stock (Show)

pcli :: Parser Cli
pcli = do
  rootPath <- strOption (help "Where config and lock files are located" <> long "root" <> metavar "ROOT" <> value "etc")
  verbose <- switch (long "verbose" <> short 'v')
  action <- paction
  pure Cli {rootPath, verbose, action}

data Action
  = -- | Update a service
    Update {staticId :: StaticId}
  | -- | Switch a service or state to another version
    Switch {staticId :: StaticId, version :: Text, dynamics :: DynamicsSelection}
  deriving stock (Generic, Show)

paction :: Parser Action
paction = subparser $ command "update" (info pupdate idm) <> command "switch" (info pswitch idm)
  where
    pupdate = do
      staticId <- strArgument (help "id of a static to update" <> metavar "STATIC_ID")
      pure Update {staticId}
    pswitch = do
      staticId <- strArgument (help "id of a static to switch" <> metavar "STATIC_ID")
      version <- strArgument (help "new version (revision)" <> metavar "STATIC_VERSION")
      dynamics <- pdynamicsSelection
      pure Switch {staticId, version, dynamics}

data DynamicsSelection
  = WithAll
  | Manual {with, without :: Set DynamicId}
  deriving stock (Generic, Show)

pdynamicsSelection :: Parser DynamicsSelection
pdynamicsSelection = pwithAll <|> pmanual
  where
    pwithAll = flag' WithAll (help "Backup all dynamics" <> long "with-all")
    pmanual = do
      with <- fmap fromList . many . strOption $ long "with"
      without <- fmap fromList . many . strOption $ long "without"
      pure Manual {with, without}
