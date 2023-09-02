module Memento.Cli where

import Memento.Types.Dynamic (DynamicId)
import Memento.Types.Static (StaticId)
import Options.Applicative (Parser, command, flag', help, idm, info, long, metavar, short, strArgument, strOption, subparser, switch, value)

data Cli = Cli
  { -- , comment :: Maybe Text
    verbose :: Bool
  , environment :: Environment
  }
  deriving stock (Show)

pcli :: Parser Cli
pcli = do
  verbose <- switch $ long "verbose" <> short 'v'
  environment <- penvironment
  pure Cli {verbose, environment}

data Environment
  = -- | Runtime
    System {systemAction :: SystemAction}
  | -- | "Config-time"
    Config {etc :: FilePath, configAction :: ConfigAction}
  deriving stock (Show)

penvironment :: Parser Environment
penvironment = subparser $ command "system" (info psystem idm) <> command "config" (info pconfig idm)
  where
    psystem = do
      systemAction <- psystemAction
      pure System {systemAction}
    pconfig = do
      etc <- strOption $ help "path to directory with config and lock" <> long "etc" <> value "./etc"
      configAction <- pconfigAction
      pure Config {etc, configAction}

data SystemAction
  = -- | Update system with new config and lock
    Upgrade {newEtc, newBuiltPath :: FilePath}
  | -- | Switch a service or state to another version
    Switch {staticId :: StaticId, version :: Text, dynamicsSelection :: DynamicsSelection}
  deriving stock (Show)

psystemAction :: Parser SystemAction
psystemAction = subparser $ command "upgrade" (info pupgrade idm) <> command "switch" (info pswitch idm)
  where
    pupgrade = do
      newEtc <- strOption $ help "path to directory with NEW config and lock" <> long "new-etc"
      newBuiltPath <- strOption $ help "path to NEW \"built\" lock file" <> long "new-built"
      pure Upgrade {newEtc, newBuiltPath}

    pswitch = do
      staticId <- strArgument $ help "id of a static to switch" <> metavar "STATIC_ID"
      version <- strArgument $ help "new version (revision)" <> metavar "STATIC_VERSION"
      dynamicsSelection <- pdynamicsSelection
      pure Switch {staticId, version, dynamicsSelection}

data DynamicsSelection
  = WithAll
  | Manual {with, without :: Set DynamicId}
  deriving stock (Show)

pdynamicsSelection :: Parser DynamicsSelection
pdynamicsSelection = pwithAll <|> pmanual
  where
    pwithAll = flag' WithAll $ help "Backup all dynamics" <> long "with-all"
    pmanual = do
      with <- fmap fromList . many . strOption $ help "backup this dynamic before update" <> long "with"
      without <- fmap fromList . many . strOption $ long "without"
      pure Manual {with, without}

data ConfigAction
  = -- | Open and parse config and lock files
    Check
  | -- | Update a service
    Update {staticId :: StaticId}
  deriving stock (Show)

pconfigAction :: Parser ConfigAction
pconfigAction = subparser $ command "check" (info pcheck idm) <> command "update" (info pupdate idm)
  where
    pcheck = pure Check

    pupdate = do
      staticId <- strArgument $ help "id of a static to update" <> metavar "STATIC_ID"
      pure Update {staticId}
