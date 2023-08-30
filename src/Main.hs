module Main where

import Control.Lens (Ixed (ix), Lens, Lens', Prism, Prism', at, lens, preview, re, review, (%%=), (%=), (%~), (^?), _Just)
import Data.Aeson (FromJSON, Object, eitherDecode, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens (_String)
import Data.Default.Class (Default (def))
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Dynamic qualified as Eff (Error, runError, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist, runFileSystem)
import Effectful.Process (Process, readProcess, runProcess)
import Effectful.Reader.Dynamic qualified as Eff (Reader, runReader)
import Effectful.State.Dynamic qualified as Eff (State (..), execStateShared)
import Memento.Cli (Action (..), Cli (..), pcli)
import Memento.Types.Config (ConfigFile, subjectsL)
import Memento.Types.Config qualified as Config (_Static)
import Memento.Types.Lock (LockFile, locksL)
import Memento.Types.Static (StaticConfig (..), StaticId (StaticId), StaticLock (..), StaticLocks, StaticSource (..), StaticVersion (..))
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

decodeJson :: (FromJSON a, IOE :> r, Eff.Error Text :> r) => FilePath -> Eff r a
decodeJson p = liftIO (eitherDecodeFileStrict p) >>= either fail' pure

main :: IO ()
main = runMain do
  cli <- liftIO $ execParser $ info (pcli <**> helper) fullDesc
  let
    lockFilePath = cli.rootPath </> "memento.lock.json"
    configFilePath = cli.rootPath </> "memento.json"
  config <- decodeJson configFilePath
  lock <- decodeJsonOrEmpty lockFilePath
  newLock <- Eff.runReader config do
    Eff.execStateShared lock do
      run cli
  writeFileLBS lockFilePath $ encodePretty newLock
  where
    runMain = runEff . runErrorWith putTextLn . runFileSystem . runProcess

runErrorWith :: (IOE :> r) => (e -> Eff r ()) -> Eff (Eff.Error e : r) a -> Eff r a
runErrorWith f = Eff.runError >=> either onError pure
  where
    onError (cs, err) = do
      f err
      putStrLn $ prettyCallStack cs
      exitFailure

runIO :: Action -> IO ()
runIO action = runEff . runErrorWith putTextLn . runProcess $ runFileSystem do
  let
    lockFilePath = "etc" </> "memento.lock.json"
    configFilePath = "etc" </> "memento.json"
  config <- decodeJson configFilePath
  lock <- decodeJsonOrEmpty lockFilePath
  newLock <- Eff.runReader config do
    Eff.execStateShared lock do
      run Cli {rootPath = "etc", verbose = True, action}
  writeFileLBS lockFilePath $ encodePretty newLock

run :: (Eff.Reader ConfigFile :> r, Eff.State LockFile :> r, Process :> r, Eff.Error Text :> r, IOE :> r) => Cli -> Eff r ()
run Cli {action} = case action of
  Update {staticId} -> do
    StaticConfig {source} <-
      preview (subjectsL . ix (coerce staticId) . Config._Static) >>= \case
        Nothing -> fail' @Text $ "No static with id = " <> show staticId <> " was found"
        Just ok -> pure ok
    locked <- fetchSource source
    let newLock = StaticLock {original = source, locked}
    changed <-
      locksL . at (coerce staticId) . orDefault [] _Just %%= \case
        xs@(h : _) | h == newLock -> (False, xs)
        xs -> (True, newLock : xs)
    unless changed do
      putTextLn "Fresh version is already locked, lock file was not changed"
  Switch {staticId, version, dynamics} -> pure ()

orDefault :: a -> Prism' s a -> Lens' s a
orDefault d p = lens (fromMaybe d . preview p) (const (review p))

fetchSource :: (Process :> r, Eff.Error Text :> r) => StaticSource -> Eff r StaticVersion
fetchSource Git {url, ref} = nixPrefetchGit url ref

nixPrefetchGit :: (Process :> r, Eff.Error Text :> r) => Text -> Text -> Eff r StaticVersion
nixPrefetchGit url ref = do
  raw <- readProcess "nix-prefetch-git" ["--url", toString url, "--rev", toString ref] mempty
  value :: Object <- either fail' pure . eitherDecode . toLazy $ encodeUtf8 raw
  rev <- maybe (fail' @Text "`rev` not found in nix-prefetch-git output") pure $ value ^? ix "rev" . _String
  sha256 <- maybe (fail' @Text "`sha256` not found in nix-prefetch-git output") pure $ value ^? ix "sha256" . _String
  pure GitVersion {rev, sha256}
