module Main where

import Colog (Message, Severity (Debug, Error, Info), simpleMessageAction)
import Control.Lens (Ixed (ix), Lens', Prism', at, from, lens, preview, review, (%%=), (^?), _Just, _head)
import Data.Aeson (FromJSON, Object, eitherDecode, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens (_String)
import Data.Default.Class (Default (def))
import Data.Map.Extra qualified as Map (zip)
import Data.Map.Strict ((!?))
import Data.Map.Strict qualified as Map (toList)
import Data.Set ((\\))
import Data.Set qualified as Set (union)
import Data.Text qualified as Text (intercalate)
import Data.These (These (..))
import Data.Vector qualified as Vector (zip)
import Data.Vector.Lens (vector)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Colog.Dynamic (Logger, log, runLogAction)
import Effectful.Error.Dynamic qualified as Eff (Error, runError, throwError)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, createFileLink, doesFileExist, doesPathExist, makeAbsolute, removeFile, runFileSystem)
import Effectful.Process (Process, callProcess, readProcess, runProcess)
import Effectful.Reader.Dynamic qualified as Eff (Reader, runReader)
import Effectful.State.Dynamic qualified as Eff (State (..), execStateShared)
import Memento.Cli (Cli (..), ConfigAction (..), DynamicsSelection (..), Environment (..), SystemAction (..), pcli)
import Memento.Types.Built (BuiltDoc (..), BuiltLock (BuiltLock, built))
import Memento.Types.Common (SubjectId (SubjectId))
import Memento.Types.Config (ConfigDoc (..), SubjectConfig (Dynamic, Static), subjectsL)
import Memento.Types.Config qualified as Config (_Static)
import Memento.Types.Lock (LockDoc (..), locksL)
import Memento.Types.Static (StaticConfig (..), StaticId (StaticId), StaticLock (..), StaticSource (..), StaticVersion (..))
import Options.Applicative (execParser, fullDesc, helper, info)
import Orphans ()
import System.FilePath ((</>))

fail' :: (ToText s, HasCallStack, Eff.Error Text :> r) => s -> Eff r a
fail' e = withFrozenCallStack do
  Eff.throwError (toText e)

decodeJsonOrEmpty :: forall a r. (HasCallStack, IOE :> r, FileSystem :> r, Default a, FromJSON a, Eff.Error Text :> r) => FilePath -> Eff r a
decodeJsonOrEmpty p = do
  doesFileExist p >>= \case
    True -> liftIO (eitherDecodeFileStrict p) >>= either fail' pure
    False -> pure def

decodeJson :: (HasCallStack, FromJSON a, IOE :> r, Eff.Error Text :> r) => FilePath -> Eff r a
decodeJson p = liftIO (eitherDecodeFileStrict p) >>= either fail' pure

main :: (HasCallStack) => IO ()
main = runMain do
  cli <- liftIO $ execParser $ info (pcli <**> helper) fullDesc
  run cli
  where
    runMain = runEff . runLogAction simpleMessageAction . runErrorLogging . runFileSystem . runProcess

runErrorLogging :: (IOE :> r, Logger Message :> r) => Eff (Eff.Error Text : r) a -> Eff r a
runErrorLogging = Eff.runError >=> either onError pure
  where
    onError (cs, err) = do
      log Error err
      log Error $ toText $ prettyCallStack cs
      exitFailure

run :: (HasCallStack, Process :> r, Eff.Error Text :> r, IOE :> r, FileSystem :> r, Logger Message :> r) => Cli -> Eff r ()
run Cli {environment} = case environment of
  System {systemAction} ->
    let
      root = "/nix/var/nix/gcroots/memento/"
      etc = root </> "etc"
      static = root </> "static"
      pathFor (StaticId s) = static </> show s
      currentPathFor s = pathFor s </> "current"
      configFilePath = etc </> "memento.json"
      lockFilePath = etc </> "memento.lock.json"
      builtFilePath = etc </> "memento.built.json"
     in
      case systemAction of
        Upgrade {newEtc, newBuiltPath} -> do
          let
            newConfigFilePath = newEtc </> "memento.json"
            newLockFilePath = newEtc </> "memento.lock.json"

          newConfig :: ConfigDoc <- decodeJson newConfigFilePath
          newLock :: LockDoc <- decodeJson newLockFilePath
          newBuilt :: BuiltDoc <- decodeJson newBuiltPath

          for_ @[] [root, etc, static] $ createDirectoryIfMissing False

          lock :: LockDoc <- decodeJsonOrEmpty lockFilePath

          for_ (Map.toList $ Map.zip lock.locks newLock.locks) \case
            (staticId, This _obsolete) -> do
              log Info $ "Not removing obsolete static " <> show staticId
              log Info $ "To remove it manually, run `rm -r " <> toText (pathFor staticId) <> "`"
            (staticId, That _fresh) -> do
              log Info $ "Creating directory for fresh static " <> show staticId
              createDirectoryIfMissing False $ pathFor staticId
              BuiltLock {built} <-
                newBuilt.locks !? staticId <! "Built-file does not contain " <> show staticId
                  >>= preview _head .! "Built-file does not contain any versions for " <> show staticId
              createFileLinkLogging built $ currentPathFor staticId
              systemdRestart $ show staticId
            (staticId, These old new)
              | old == new -> log Debug $ "Static " <> show staticId <> " is unchanged"
              | otherwise -> case newConfig.subjects !? coerce staticId of
                  Nothing -> fail' $ "New config at " <> toText newConfigFilePath <> " misses entry for static " <> show staticId
                  Just (Dynamic _) -> fail' @Text $ "New config lists " <> show staticId <> " as dynamic"
                  Just (Static StaticConfig {upgradeOnNewVersion}) ->
                    if upgradeOnNewVersion
                      then do
                        log Info $ "Upgrading " <> show staticId
                        BuiltLock {built} <-
                          newBuilt.locks !? staticId <! "Built-file does not contain " <> show staticId
                            >>= preview _head .! "Built-file does not contain any versions for " <> show staticId
                        replaceFileLinkLogging built $ currentPathFor staticId
                        systemdRestart $ show staticId
                      else log Info $ "Upgrading on new version is disabled, skipping " <> show staticId

          log Info "Linking new /etc"
          replaceFileLinkLogging newConfigFilePath configFilePath
          replaceFileLinkLogging newLockFilePath lockFilePath
          replaceFileLinkLogging newBuiltPath builtFilePath
        Switch {staticId, version, dynamicsSelection} -> do
          config :: ConfigDoc <- decodeJson configFilePath
          lock :: LockDoc <- decodeJson lockFilePath
          built :: BuiltDoc <- decodeJson builtFilePath
          StaticConfig {dynamics} <-
            config.subjects !? coerce staticId <! "Config does not define " <> show staticId
              >>= preview Config._Static .! "Config does not define " <> show staticId <> " as a static"
          toBackup <-
            case dynamicsSelection of
              WithAll -> pure dynamics
              Manual {with, without} -> do
                let
                  selectedDyn = with `Set.union` without
                  allDyn = fromList (toList dynamics)
                  unspecifiedDyn = allDyn \\ selectedDyn
                  unknownDyn = selectedDyn \\ allDyn
                unless (null unknownDyn) do
                  fail' $ "Specified unknown dynamics: " <> commas (show <$> toList unknownDyn)
                unless (null unspecifiedDyn) do
                  fail' $ "Didn't specify dynamics (use --with or --without): " <> commas (show <$> toList unspecifiedDyn)
                pure $ fromList $ toList with
          thisLock <- lock.locks !? staticId <! "Lock does not contain " <> show staticId
          thisBuilt <- built.locks !? staticId <! "Built-file does not contain " <> show staticId
          BuiltLock {built = newCurrent} <-
            lookupBy ((== version) . (.locked.rev)) (Vector.zip thisLock thisBuilt)
              <! "Lock does not contain specified version of "
              <> show staticId
          unless (null toBackup) do
            log Info $ "Backing up " <> commas (show <$> toList toBackup) <> " (not really)"
          replaceFileLinkLogging newCurrent $ currentPathFor staticId
          systemdRestart $ show staticId
  Config {etc, configAction} ->
    inConfigEnv
      etc
      case configAction of
        Check -> log Info "OK"
        Update {staticId} -> do
          StaticConfig {source} <-
            preview (subjectsL . ix (coerce staticId) . Config._Static) >>= \case
              Nothing -> fail' @Text $ "No static with id = " <> show staticId <> " was found"
              Just ok -> pure ok
          locked <- fetchSource source
          let newLock = StaticLock {original = source, locked}
          changed <-
            locksL . at (coerce staticId) . orDefault mempty _Just . from vector %%= \case
              xs@(h : _) | h == newLock -> (False, xs)
              xs -> (True, newLock : xs)
          unless changed do
            putTextLn "Fresh version is already locked, lock file was not changed"

systemdRestart :: (Process :> r) => String -> Eff r ()
systemdRestart x = callProcess "systemctl" ["reload-or-try-restart", x]

commas :: [Text] -> Text
commas = Text.intercalate ", "

(<!) :: (Eff.Error e :> r) => Maybe a -> e -> Eff r a
Just x <! _ = pure x
Nothing <! e = Eff.throwError e

(.!) :: (Eff.Error e :> r) => (a -> Maybe b) -> e -> a -> Eff r b
f .! e = (<! e) . f

infix 5 <!, .!

createFileLinkLogging :: (Logger Message :> r, FileSystem :> r) => FilePath -> FilePath -> Eff r ()
createFileLinkLogging source target = do
  source' <- makeAbsolute source
  log Info $ toText $ target <> " -> " <> source'
  createFileLink source' target

replaceFileLinkLogging :: (Logger Message :> es, FileSystem :> es) => FilePath -> FilePath -> Eff es ()
replaceFileLinkLogging source target = do
  doesPathExist target >>= \case
    True -> do
      log Debug $ toText target <> " exists, removing"
      removeFile target
    False -> mempty
  createFileLinkLogging source target

inConfigEnv ::
  (HasCallStack, Eff.Error Text :> r, IOE :> r, FileSystem :> r) =>
  FilePath ->
  Eff (Eff.State LockDoc : Eff.Reader ConfigDoc : r) a2 ->
  Eff r ()
inConfigEnv etc act = do
  let
    lockFilePath = etc </> "memento.lock.json"
    configFilePath = etc </> "memento.json"
  config <- decodeJson configFilePath
  lock <- decodeJsonOrEmpty lockFilePath
  newLock <- Eff.runReader config do
    Eff.execStateShared lock do
      act
  writeFileLBS lockFilePath $ encodePretty newLock

lookupBy :: (Foldable f) => (a -> Bool) -> f (a, b) -> Maybe b
lookupBy f = getFirst . foldMap \(k, v) -> First if f k then Just v else Nothing

orDefault :: a -> Prism' s a -> Lens' s a
orDefault d p = lens (fromMaybe d . preview p) (const (review p))

fetchSource :: (HasCallStack, Process :> r, Eff.Error Text :> r) => StaticSource -> Eff r StaticVersion
fetchSource Git {url, ref} = nixPrefetchGit url ref

nixPrefetchGit :: (HasCallStack, Process :> r, Eff.Error Text :> r) => Text -> Text -> Eff r StaticVersion
nixPrefetchGit url ref = do
  raw <- readProcess "nix-prefetch-git" ["--url", toString url, "--rev", toString ref] mempty
  value :: Object <- either fail' pure . eitherDecode . toLazy $ encodeUtf8 raw
  rev <- maybe (fail' @Text "`rev` not found in nix-prefetch-git output") pure $ value ^? ix "rev" . _String
  sha256 <- maybe (fail' @Text "`sha256` not found in nix-prefetch-git output") pure $ value ^? ix "sha256" . _String
  pure GitVersion {rev, sha256}
