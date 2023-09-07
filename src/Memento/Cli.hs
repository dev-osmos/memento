module Memento.Cli where

import GHC.TypeLits (KnownSymbol, symbolVal)
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
    Switch
      { staticId :: StaticId
      , version :: Text
      , saveDynamics :: Selection "save" DynamicId
      , restoreDynamics :: Maybe (Selection "restore" DynamicId)
      }
  | Rollback
      { staticId :: StaticId
      , saveDynamics :: Selection "save" DynamicId
      , restoreDynamicsRollback :: Selection "restore" DynamicId
      }
  deriving stock (Show)

psystemAction :: Parser SystemAction
psystemAction = subparser $ command "upgrade" (info pupgrade idm) <> command "switch" (info pswitch idm) <> command "rollback" (info prollback idm)
  where
    pupgrade = do
      newEtc <- strOption $ help "path to directory with NEW config and lock" <> long "new-etc"
      newBuiltPath <- strOption $ help "path to NEW \"built\" lock file" <> long "new-built"
      pure Upgrade {newEtc, newBuiltPath}

    pswitch = do
      staticId <- strArgument $ help "id of a static to switch" <> metavar "STATIC_ID"
      version <- strArgument $ help "new version (revision)" <> metavar "STATIC_VERSION"
      saveDynamics <- pselection "save this dynamic before switching"
      restoreDynamics <- optional $ pselection "restore this dynamic before switching"
      pure Switch {staticId, version, saveDynamics, restoreDynamics}

    prollback = do
      staticId <- strArgument $ help "id of a static to rollback" <> metavar "STATIC_ID"
      saveDynamics <- pselection "save this dynamic before switching"
      restoreDynamicsRollback <- pselection "restore this dynamic before rolling back"
      pure Rollback {staticId, saveDynamics, restoreDynamicsRollback}

data Selection act a
  = SelectAll
  | Select {with, without :: Set a}
  deriving stock (Show)

pselection :: forall act a. (KnownSymbol act, IsString a, Ord a) => String -> Parser (Selection act a)
pselection actionHelp = pall <|> pselect
  where
    action = symbolVal (Proxy @act)
    pall = flag' SelectAll $ help (action <> " all") <> long (action <> "-all")
    pselect = do
      with <- fmap fromList . many . strOption $ help actionHelp <> long action
      without <- fmap fromList . many . strOption $ long ("no-" <> action)
      pure Select {with, without}

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
