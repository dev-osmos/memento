module Memento.System where

import Amazonka (Env, _Time)
import Colog (Message, Severity (Debug, Info))
import Control.Lens (from, ix, preview, (%~), (^.), (^?), _head, _last)
import Data.Map.Extra qualified as Map (zip)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map (toList)
import Data.Set ((\\))
import Data.Set qualified as Set (intersection, union)
import Data.These (These (..))
import Data.Time (getCurrentTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (findIndex, zip)
import Effectful (Eff, IOE, (:>))
import Effectful.Break (runBreakHomo)
import Effectful.Break qualified as Eff (break)
import Effectful.Colog.Dynamic (Logger, log)
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Dynamic qualified as Eff (Error)
import Effectful.Error.Dynamic.Extra ((.!), (<!))
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, getSymbolicLinkTarget)
import Effectful.Process (Process)
import Effectful.Reader.Dynamic qualified as Eff (Reader)
import Effectful.Transaction (addAbortHandler, runTransaction, (.!!), (<!!))
import Memento.Cli (Selection (..), SystemAction (..), withNull)
import Memento.Types.Built (BuiltDoc (..), BuiltLock (BuiltLock, built))
import Memento.Types.Common (SubjectId (SubjectId))
import Memento.Types.Config (ConfigDoc (..))
import Memento.Types.Config qualified as Config (_Dynamic, _Static)
import Memento.Types.Dynamic (DynamicId (DynamicId), restore)
import Memento.Types.History (HistoryDoc (..), TimeInterval (until), historyL, initHistory)
import Memento.Types.History qualified as History (contains, currentVersion, initHistory, switch, versionActiveTimeIntervals, versions, _Finite)
import Memento.Types.Lock (LockDoc (..))
import Memento.Types.Static (StaticConfig (..), StaticId (StaticId), StaticLock (..), StaticVersion (..), lockedL)
import Orphans ()
import System.Exit (ExitCode)
import System.FilePath ((</>))
import Utils

root, etc, static, configFilePath, lockFilePath, builtFilePath :: FilePath
root = "/nix/var/nix/gcroots/memento/"
etc = root </> "etc"
static = root </> "static"
configFilePath = etc </> "memento.json"
lockFilePath = etc </> "memento.lock.json"
builtFilePath = etc </> "memento.built.json"

pathFor, currentPathFor, historyPathFor :: StaticId -> FilePath
pathFor (StaticId s) = static </> show s
currentPathFor s = pathFor s </> "current"
historyPathFor s = pathFor s </> "history.json"

run :: (HasCallStack, Process :> r, Eff.Error Text :> r, IOE :> r, FileSystem :> r, Logger Message :> r, Eff.Reader Env :> r, Concurrent :> r) => SystemAction -> Eff r ()
run Upgrade {newEtc, newBuiltPath} = do
  let
    newConfigFilePath = newEtc </> "memento.json"
    newLockFilePath = newEtc </> "memento.lock.json"

  newConfig :: ConfigDoc <- decodeJsonDoc newConfigFilePath
  newLock :: LockDoc <- decodeJsonDoc newLockFilePath
  newBuilt :: BuiltDoc <- decodeJsonDoc newBuiltPath

  for_ @[] [root, etc, static] $ createDirectoryIfMissing False

  lock :: LockDoc <- decodeJsonDocOrEmpty lockFilePath

  for_ (Map.toList $ Map.zip lock.locks newLock.locks) \(staticId, states) -> do
    mapError (\(c :: ExitCode) -> "systemd exited with code = " <> show c) $ runTransaction do
      case states of
        This _obsolete -> do
          log Info $ "Not removing obsolete static " <> show staticId
          log Info $ "To remove it manually, run `rm -r " <> toText (pathFor staticId) <> "`"
        -- when isSystemdService do
        --   systemd "stop" staticId
        That fresh -> do
          log Info $ "Creating directory for fresh static " <> show staticId
          createDirectoryIfMissing False $ pathFor staticId
          genesis <- fresh ^? _head . lockedL <! "Built-file does not contain any versions for " <> show staticId
          BuiltLock {built} <-
            newBuilt.locks
              !? staticId
              <! "Built-file does not contain "
              <> show staticId
              >>= preview _head
              .! "Built-file does not contain any versions for "
              <> show staticId
          replaceFileLinkLogging built $ currentPathFor staticId
          encodeJsonDoc (historyPathFor staticId) $ History.initHistory genesis
        -- when isSystemdService do
        --   systemd "start" staticId
        These old new
          | old == new -> log Debug $ "Static " <> show staticId <> " is unchanged"
          | otherwise -> do
              StaticConfig {upgradeOnNewVersion} <-
                newConfig.subjects
                  !? coerce staticId
                  <! "Config does not contain "
                  <> show staticId
                  >>= preview Config._Static
                  .! "Config does not define "
                  <> show staticId
                  <> " as a static"
              if upgradeOnNewVersion
                then do
                  newVersion <- new ^? _head . lockedL <! "Lock for " <> show staticId <> " is empty"
                  thisBuilt <- newBuilt.locks !? staticId <! "Built-file does not contain " <> show staticId
                  history :: HistoryDoc <- decodeJsonDoc (historyPathFor staticId)
                  switch staticId newVersion newConfig new thisBuilt (Just history {-SelectAll-}) Nothing
                else log Info $ "Upgrading on new version is disabled, skipping " <> show staticId

  log Info "Linking new /etc"
  replaceFileLinkLogging newConfigFilePath configFilePath
  replaceFileLinkLogging newLockFilePath lockFilePath
  replaceFileLinkLogging newBuiltPath builtFilePath
run Switch {staticId, version {-saveDynamics,-}, restoreDynamics} = do
  config :: ConfigDoc <- decodeJsonDoc configFilePath
  lock :: LockDoc <- decodeJsonDoc lockFilePath
  built :: BuiltDoc <- decodeJsonDoc builtFilePath
  thisLock <- lock.locks !? staticId <! "Lock does not contain " <> show staticId
  thisBuilt <- built.locks !? staticId <! "Built-file does not contain " <> show staticId
  version' <- foldFirstEq ((== version) . (.rev)) ((.locked) <$> thisLock) <! ""
  history :: HistoryDoc <- decodeJsonDocOr (historyPathFor staticId) $ initHistory version'
  switch staticId version' config thisLock thisBuilt (Just history {-saveDynamics-}) restoreDynamics
run Rollback {staticId {-saveDynamics,-}, restoreDynamicsRollback} = do
  config :: ConfigDoc <- decodeJsonDoc configFilePath
  lock :: LockDoc <- decodeJsonDoc lockFilePath
  built :: BuiltDoc <- decodeJsonDoc builtFilePath
  history :: HistoryDoc <- decodeJsonDoc (historyPathFor staticId)
  thisLock <- lock.locks !? staticId <! "Lock does not contain " <> show staticId
  thisBuilt <- built.locks !? staticId <! "Built-file does not contain " <> show staticId
  let currentVersion = History.currentVersion history.history
      versions = History.versions history.history
      -- \*First* occurence of current version in history
      currentVersionIdx = versions & Vector.findIndex (== currentVersion) & fromMaybe (error "impossible: currentVersion was not found among versions")
  -- Parent of first occurence of current version is the target version
  newVersion <- versions ^? ix (currentVersionIdx - 1) <! "Cannot rollback " <> show staticId <> " any further: already at genesis"
  switch staticId newVersion config thisLock thisBuilt (Just history {-saveDynamics-}) (Just restoreDynamicsRollback)

switch ::
  forall r s'.
  (HasCallStack, Eff.Error Text :> r, IOE :> r, FileSystem :> r, Logger Message :> r, Process :> r, Eff.Reader Env :> r, Concurrent :> r) =>
  StaticId ->
  StaticVersion ->
  ConfigDoc ->
  Vector StaticLock ->
  Vector BuiltLock ->
  Maybe HistoryDoc ->
  -- Selection s DynamicId ->
  Maybe (Selection s' DynamicId) ->
  Eff r ()
switch staticId version config lock built historyMb {-saveDynamics-} restoreDynamics = runBreakHomo do
  for_ historyMb \history -> do
    when (History.currentVersion history.history == version) do
      log Info $ "Version " <> show version <> " is already last"
      Eff.break ()
  let
    isRollback = maybe False (History.contains version . (.history)) historyMb
  StaticConfig {isSystemdService, forceReloadOrTryRestart, dynamics} <-
    config.subjects
      !? coerce staticId
      <! "Config does not define "
      <> show staticId
      >>= preview Config._Static
      .! "Config does not define "
      <> show staticId
      <> " as a static"
  let
    dynamics' :: Set DynamicId
    dynamics' = fromList $ toList dynamics
  -- toSave = with' dynamics' saveDynamics

  -- selectionIsComplete "dynamics" dynamics' saveDynamics
  when (isRollback && isNothing restoreDynamics) do
    fail' @Text "Action is a rollback, but restore selection wasn't specified"
  when (not isRollback && isJust restoreDynamics) do
    fail' @Text "Action is not a rollback, but restore selection was specified"
  let restoreIsEmpty = null dynamics' || maybe True withNull restoreDynamics
  -- (restoreAction :: Maybe (Eff r ())) <- for restoreDynamics \rd -> do
  --   selectionIsComplete "dynamics" dynamics' rd
  --   let toRestore = toList $ with' dynamics' rd
  --   -- each but 1 restored dynamic must be also saved
  --   restoreActions <- for toRestore \dynamicId -> do
  --     let isSaved = Set.member dynamicId toSave
  --     pure (isSaved, _restoreAction)
  --   pure $ traverse_ snd restoreActions

  let
    restoreDynamicsOrEmpty = maybe [] (toList . with' dynamics') restoreDynamics
  -- toRestore :: [(isSaved, (dynamicId, restoreAction))]
  -- toRestore <- for restoreDynamicsOrEmpty \dynamicId -> do
  --   let isSaved = Set.member dynamicId toSave
  --   let restoreAction = do
  --         _1
  --   pure (isSaved, (dynamicId, restoreAction))
  -- let
  --   (saved, notSaved) = fmap2 (fmap snd) $ partition fst toRestore

  -- when (length notSaved > 1) do
  --   Eff.throwError $ "At most 1 dynamic can be restored and not saved, got: " <> commas (coerce @DynamicId . fst <$> notSaved)

  BuiltLock {built = newCurrent} <-
    lookupBy ((== version) . (.locked)) (Vector.zip lock built)
      <! "Lock does not contain specified version of "
      <> show staticId

  -- and action
  runTransaction do
    -- unless (null toSave) do
    --   log Info $ "Saving " <> commas (show <$> toList toSave)

    oldCurrent <- getSymbolicLinkTarget (currentPathFor staticId)
    addAbortHandler \_ -> do
      replaceFileLinkLogging oldCurrent $ currentPathFor staticId
      systemd "reload-or-try-restart" staticId
    replaceFileLinkLogging newCurrent $ currentPathFor staticId

    when isSystemdService do
      let reloadOrTryRestart = forceReloadOrTryRestart || restoreIsEmpty
      unless reloadOrTryRestart do
        systemd "stop" staticId
      when isRollback do
        versionActiveLastInterval <-
          historyMb
            <!! "history is Nothing"
            >>= preview _last . History.versionActiveTimeIntervals version . (.history) .!! "version was not active before"
        versionActiveLastTime <- versionActiveLastInterval.until ^? History._Finite <!! "last interval has not ended"

        for_ restoreDynamicsOrEmpty \dynamicId -> do
          log Info $ "Restoring " <> show dynamicId
          dynamicConfig <-
            config.subjects
              !? coerce dynamicId
              <! "Config does not contain "
              <> show dynamicId
              >>= preview Config._Dynamic
              .! "Config does not define "
              <> show dynamicId
              <> " as a dynamic"
          restore dynamicConfig (versionActiveLastTime ^. _Time)
      if reloadOrTryRestart
        then systemd "reload-or-try-restart" staticId
        else systemd "start" staticId
  now <- liftIO getCurrentTime
  let
    newHistory = case historyMb of
      Nothing -> initHistory version
      Just h -> h & historyL %~ snd . History.switch (now ^. from _Time) version
  encodeJsonDoc (historyPathFor staticId) newHistory

selectionIsComplete :: (HasCallStack, Ord a, Show a, Eff.Error Text :> r) => Text -> Set a -> Selection act a -> Eff r ()
selectionIsComplete _ _ SelectAll = mempty
selectionIsComplete name whole Select {with, without} = do
  let
    selected = with `Set.union` without
    selectedTwice = with `Set.intersection` without
    unspecified = whole \\ selected
    unknown = selected \\ whole
  unless (null unknown) do
    fail' $ "Specified unknown " <> name <> ": " <> commas (show <$> toList unknown)
  unless (null unspecified) do
    fail' $ "Didn't specify " <> name <> " (use --with or --without): " <> commas (show <$> toList unspecified)
  unless (null selectedTwice) do
    fail' $ "Specified twice: " <> commas (show <$> toList selectedTwice)

with' :: Set a -> Selection act a -> Set a
with' whole SelectAll = whole
with' _ Select {with} = with
