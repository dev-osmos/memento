{-# LANGUAGE ImplicitParams #-}

module Utils where

import Colog (Message, Severity (Debug, Error, Info))
import Control.Lens (Ixed (ix), Lens', Prism', lens, preview, review, (^?))
import Data.Aeson (FromJSON, Object, ToJSON, eitherDecode, eitherDecodeFileStrict, encodeFile)
import Data.Aeson.Lens (_String)
import Data.Composition ((.:))
import Data.Default.Class (Default (def))
import Data.Text qualified as Text (intercalate)
import Effectful (Eff, IOE, (:>))
import Effectful.Colog.Dynamic (Logger, log)
import Effectful.Error.Dynamic qualified as Eff (Error, runError, throwError)
import Effectful.FileSystem (FileSystem, createFileLink, doesFileExist, doesPathExist, makeAbsolute, removeFile)
import Effectful.Process (Process, readProcess, spawnProcess, waitForProcess)
import Effectful.Transaction (Transaction, abort)
import Memento.Types.Static (StaticId, StaticSource (..), StaticVersion (..))
import Orphans ()
import System.Exit (ExitCode (..))

fail' :: (ToText s, HasCallStack, Eff.Error Text :> r) => s -> Eff r a
fail' e = withFrozenCallStack do
  Eff.throwError (toText e)

decodeJsonDocOr :: forall a r. (HasCallStack, IOE :> r, FileSystem :> r, FromJSON a, Eff.Error Text :> r) => FilePath -> a -> Eff r a
decodeJsonDocOr p x = do
  doesFileExist p >>= \case
    True -> liftIO (eitherDecodeFileStrict p) >>= either fail' pure
    False -> pure x

decodeJsonDocOrEmpty :: forall a r. (HasCallStack, IOE :> r, FileSystem :> r, FromJSON a, Default a, Eff.Error Text :> r) => FilePath -> Eff r a
decodeJsonDocOrEmpty p = decodeJsonDocOr p def

decodeJsonDoc :: (HasCallStack, FromJSON a, IOE :> r, Eff.Error Text :> r) => FilePath -> Eff r a
decodeJsonDoc p = liftIO (eitherDecodeFileStrict p) >>= either fail' pure

encodeJsonDoc :: (ToJSON a, IOE :> r) => FilePath -> a -> Eff r ()
encodeJsonDoc = liftIO .: encodeFile

foldFirst :: (Foldable f) => (a -> Maybe b) -> f a -> Maybe b
foldFirst f = getFirst . foldMap (First . f)

foldFirstEq :: (Foldable f) => (a -> Bool) -> f a -> Maybe a
foldFirstEq f = foldFirst \a -> if f a then Just a else Nothing

lookupBy :: (Foldable f) => (a -> Bool) -> f (a, b) -> Maybe b
lookupBy f = fmap snd . foldFirstEq (f . fst)

orDefault :: a -> Prism' s a -> Lens' s a
orDefault d p = lens (fromMaybe d . preview p) (const (review p))

fetchSource :: (HasCallStack, Process :> r, Eff.Error Text :> r) => StaticSource -> Eff r StaticVersion
fetchSource Git {url, ref} = nixPrefetchGit url ref

nixPrefetchGit :: (HasCallStack, Process :> r, Eff.Error Text :> r) => Text -> Text -> Eff r StaticVersion
nixPrefetchGit url ref = do
  raw <- readProcess "nix-prefetch-git" ["--url", toString url, "--rev", toString ref] mempty
  value :: Object <- either fail' pure . eitherDecode . toLazy $ encodeUtf8 raw
  rev <- value ^? ix "rev" . _String <! "`rev` not found in nix-prefetch-git output"
  sha256 <- value ^? ix "sha256" . _String <! "`sha256` not found in nix-prefetch-git output"
  pure GitVersion {rev, sha256}

systemd :: (Process :> r, Transaction ExitCode :> r) => String -> StaticId -> Eff r ()
systemd op x = do
  p <- spawnProcess "systemctl" [op, show x]
  e <- waitForProcess p
  case e of
    ExitSuccess -> mempty
    ExitFailure _ -> abort e

mapError :: (Eff.Error e :> r) => (e' -> e) -> Eff (Eff.Error e' : r) a -> Eff r a
mapError f a =
  Eff.runError a >>= \case
    Right ok -> pure ok
    Left (c, e) -> let ?callStack = c in Eff.throwError (f e)

commas :: [Text] -> Text
commas = Text.intercalate ", "

-- (<!) :: (HasCallStack, Eff.Error e :> r) => Maybe a -> e -> Eff r a
(<!) :: (HasCallStack, Eff.Error Text :> r) => Maybe a -> Text -> Eff r a
Just x <! _ = pure x
Nothing <! e = Eff.throwError e

-- (.!) :: (HasCallStack, Eff.Error e :> r) => (a -> Maybe b) -> e -> a -> Eff r b
(.!) :: (HasCallStack, Eff.Error Text :> r) => (a -> Maybe b) -> Text -> a -> Eff r b
f .! e = (<! e) . f

infix 5 <!, .!

runErrorLogging :: (HasCallStack, IOE :> r, Logger Message :> r) => Eff (Eff.Error Text : r) a -> Eff r a
runErrorLogging = Eff.runError >=> either onError pure
  where
    onError (cs, err) = do
      log Error err
      log Error $ toText $ prettyCallStack cs
      exitFailure

replaceFileLinkLogging :: (HasCallStack, Logger Message :> es, FileSystem :> es) => FilePath -> FilePath -> Eff es ()
replaceFileLinkLogging source target = do
  doesPathExist target >>= \case
    True -> do
      log Debug $ toText target <> " exists, removing"
      removeFile target
    False -> mempty
  createFileLinkLogging
  where
    createFileLinkLogging = do
      source' <- makeAbsolute source
      log Info $ toText $ target <> " -> " <> source'
      createFileLink source' target
