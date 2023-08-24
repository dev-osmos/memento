{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Chronos (now, timeToDatetime)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Default.Class (Default (def))
import Data.Foldable (maximum)
import Data.Iso (Iso (Iso))
import Data.Text qualified as Text (length, replicate)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem (FileSystem, doesFileExist, runFileSystem)
import Effectful.Reader.Dynamic qualified as Eff (Reader, runReader)
import Effectful.State.Dynamic qualified as Eff (State (..), execStateShared, modify)
import Lens.Micro ((%~))
import Lens.Micro.Mtl (use)
import Memento.Cli (Action (..), Cli (..), needsLogging)
import Memento.Types.Config (ConfigFile)
import Memento.Types.Lock (LockFile, LogEntry (..), logL)
import System.FilePath ((</>))

rootPath, lockFilePath, configFilePath :: FilePath
rootPath = "etc"
lockFilePath = rootPath </> "memento.lock.json"
configFilePath = rootPath </> "memento.json" -- TODO: TOML

decodeJsonOrEmpty :: forall a r. (IOE :> r, Fail :> r, FileSystem :> r, Default a, FromJSON a) => FilePath -> Eff r a
decodeJsonOrEmpty p = do
  doesFileExist p >>= \case
    True -> liftIO (eitherDecodeFileStrict p) >>= either fail pure
    False -> pure def

decodeJson :: (FromJSON a, IOE :> r, Fail :> r) => FilePath -> Eff r a
decodeJson p = liftIO (eitherDecodeFileStrict p) >>= either fail pure

main :: IO ()
main = runMain do
  -- config <- either (fail . _1) pure =<< eitherDecodeFileStrict configFilePath
  -- lock <- readJson @LockFile lockFilePath
  -- putLBSLn $ "config = " <> encode config
  -- putLBSLn $ "lock = " <> encode lock
  pure ()
  where
    runMain = runEff . runFailIO . runFileSystem

runIO action = runEff . runFailIO $ runFileSystem do
  config <- decodeJson configFilePath
  lock <- decodeJsonOrEmpty lockFilePath
  newLock <- Eff.runReader config do
    Eff.execStateShared lock do
      runAndLog Cli {rootPath = "etc", comment = Just "testing", verbose = True, action}
  writeFileLBS lockFilePath $ encodePretty newLock

instance (Eff.State s :> r, MonadState s (Eff r)) => MonadState s (Eff r) where
  get = send Eff.Get
  put = send . Eff.Put

run, runAndLog :: (Eff.Reader ConfigFile :> r, Eff.State LockFile :> r, IOE :> r) => Cli -> Eff r ()
runAndLog cli = do
  print cli
  x <- run cli
  when (needsLogging cli.action) do
    datetime <- liftIO $ Iso . timeToDatetime <$> now
    actor <- fmap toText <$> lookupEnv "USER"
    Eff.modify $ logL %~ (LogEntry {action = cli.action, comment = cli.comment, datetime, actor} :)
  pure x
run Cli {action = Log} = do
  log <- use logL
  traverse_ putTextLn $ mkTable $ log <&> \LogEntry {action, datetime, actor, comment} -> [show datetime, fromMaybe mempty actor, fromMaybe mempty comment, show action]
run Cli {action = Lock} = pure ()
run _ = pure ()

mkTable :: [[Text]] -> [Text]
mkTable = fmap unwords . padColumns

padColumns :: [[Text]] -> [[Text]]
padColumns rows =
  let widths :: [Int] = transpose rows <&> maximum . fmap Text.length
   in rows <&> \row -> zipWith padRight widths row

padRight :: Int -> Text -> Text
padRight width x
  | Text.length x > width = error "padRight: Text.length x > width"
  | otherwise = x <> Text.replicate (width - Text.length x) " "
