module Main where

import Colog (Message, Severity (Info), simpleMessageAction)
import Control.Lens (Ixed (ix), at, from, preview, (%%=), _Just)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Vector.Lens (vector)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Colog.Dynamic (Logger, log, runLogAction)
import Effectful.Error.Dynamic qualified as Eff (Error)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Reader.Dynamic qualified as Eff (Reader, runReader)
import Effectful.State.Dynamic qualified as Eff (State (..), execStateShared)
import Memento.Cli (Cli (..), ConfigAction (..), Environment (..), pcli)
import Memento.System qualified as System (run)
import Memento.Types.Common (SubjectId (SubjectId))
import Memento.Types.Config (ConfigDoc (..), subjectsL)
import Memento.Types.Config qualified as Config (_Static)
import Memento.Types.Lock (LockDoc (..), locksL)
import Memento.Types.Static (StaticConfig (..), StaticId (StaticId), StaticLock (..))
import Options.Applicative (execParser, fullDesc, helper, info)
import Orphans ()
import System.FilePath ((</>))
import Utils (decodeJsonDoc, decodeJsonDocOrEmpty, fail', fetchSource, orDefault, runErrorLogging)

main :: (HasCallStack) => IO ()
main = runMain do
  cli <- liftIO $ execParser $ info (pcli <**> helper) fullDesc
  run cli
  where
    runMain = runEff . runLogAction simpleMessageAction . runErrorLogging . runFileSystem . runProcess

run :: (HasCallStack, Process :> r, Eff.Error Text :> r, IOE :> r, FileSystem :> r, Logger Message :> r) => Cli -> Eff r ()
run Cli {environment} = case environment of
  System {systemAction} -> System.run systemAction
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

inConfigEnv ::
  (HasCallStack, Eff.Error Text :> r, IOE :> r, FileSystem :> r) =>
  FilePath ->
  Eff (Eff.State LockDoc : Eff.Reader ConfigDoc : r) a2 ->
  Eff r ()
inConfigEnv etc act = do
  let
    lockFilePath = etc </> "memento.lock.json"
    configFilePath = etc </> "memento.json"
  config <- decodeJsonDoc configFilePath
  lock <- decodeJsonDocOrEmpty lockFilePath
  newLock <- Eff.runReader config do
    Eff.execStateShared lock do
      act
  writeFileLBS lockFilePath $ encodePretty newLock
