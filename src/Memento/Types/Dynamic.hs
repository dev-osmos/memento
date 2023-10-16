module Memento.Types.Dynamic where

import Amazonka (Env)
import Amazonka.RDS (newDeleteDBInstance, newModifyDBInstance, newRestoreDBInstanceToPointInTime)
import Amazonka.RDS.Lens (modifyDBInstance_deletionProtection, modifyDBInstance_newDBInstanceIdentifier, restoreDBInstanceToPointInTime_restoreTime, restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier)
import Aws.Types (AvailabilityZone, RdsInstanceId (RdsInstanceId))
import Colog (Message, Severity (Warning))
import Control.Lens ((?~))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson.Local (CompositeTags (CompositeTags))
import Data.Default.Instances.Containers ()
import Data.Time (UTCTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Amazonka (sendEither)
import Effectful.Colog.Dynamic (Logger, log)
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Dynamic qualified as Eff (Reader)
import Effectful.Retry (defRC, retryTx)
import Effectful.Transaction (Transaction, (!!~))
import Memento.Types.Common (SubjectId)

newtype DynamicId = DynamicId SubjectId
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- newtype DynamicVersion = AwsRdsSnapshot {snapshotId :: RdsSnapshotId}
--   deriving stock (Generic)
--   deriving newtype (Show)
--   deriving (FromJSON, ToJSON) via (CompositeTags DynamicVersion)

data DynamicSource = AwsRdsSource {availabilityZone :: AvailabilityZone, dbInstance :: RdsInstanceId}
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via (CompositeTags DynamicSource)

newtype DynamicConfig = DynamicConfig
  { source :: DynamicSource
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

{- | Create a snapshot and return its ARN identifier
TODO: return a generalized DynamicVersion
save ::
  (HasCallStack, IOE :> r, Eff.Reader Env :> r, Transaction Text :> r, Logger Message :> r, Concurrent :> r) =>
  DynamicConfig ->
  Eff r AmazonResourceName
save config =
  retryTx defRC (unlines . toList) $
    first show <$> case config.source of
      AwsRdsSource {dbInstance} -> do
        now <- liftIO getCurrentTime
        let snapshotId = "memento-" <> coerce dbInstance <> A.toText @ISO8601 (now ^. from _Time)
            r = newCreateDBSnapshot snapshotId (coerce dbInstance)
        sendEither r >>= secondM do
          view createDBSnapshotResponse_dbSnapshot .!! "No .dbSnapshot field"
            >=> view dbSnapshot_dbSnapshotArn .!! "No .dbSnapshotArn field"
            >>> fmap ARN
-}

-- saveIfNeeded ::
--   forall r.
--   (HasCallStack, IOE :> r, Eff.Reader Env :> r, Transaction Text :> r, Logger Message :> r, Concurrent :> r) =>
--   DynamicConfig ->
--   Eff r AmazonResourceName
-- saveIfNeeded dynamicConfig = do
--   log Debug $ "Getting info about last save for " <> coerce dynamicConfig.source.dbInstance
--   savedMb <-
--     lastSaved >>= \case
--       Nothing -> do
--         log Debug "Dynamic has never been saved before"
--         -- was never saved before
--         pure Nothing
--       Just MaxBy {key = snapshotTime, value = snapshot} -> do
--         now <- liftIO getCurrentTime
--         -- was saved more than 5 minutes ago
--         let saveTooOld = toRational (now `diffUTCTime` snapshotTime) > 5 * 60
--         log Debug $ "Latest save is too old: " <> show saveTooOld <> " (= " <> show snapshotTime <> ")"
--         pure if saveTooOld then Nothing else Just snapshot
--   case savedMb of
--     Just saved -> saved ^. dbSnapshot_dbSnapshotArn <!! "No .dbSnapshotArn field" & fmap ARN
--     Nothing -> do
--       log Debug $ "Saving " <> coerce dynamicConfig.source.dbInstance
--       save dynamicConfig
--   where
--     lastSaved :: Eff r (Maybe (MaxBy UTCTime DBSnapshot))
--     lastSaved = do
--       let r = newDescribeDBSnapshots & describeDBSnapshots_dbInstanceIdentifier ?~ coerce dynamicConfig.source.dbInstance
--       runResource $ runConduit do
--         paginate r
--           .| concatMapC (fromMaybe [] . view describeDBSnapshotsResponse_dbSnapshots)
--           .| foldMapC (\value -> value ^. dbSnapshot_originalSnapshotCreateTime <&> \time -> MaxBy {key = time, value})

-- | Restore a dynamic to some point in time
restore :: (HasCallStack, IOE :> r, Eff.Reader Env :> r, Transaction Text :> r, Logger Message :> r, Concurrent :> r) => DynamicConfig -> UTCTime -> Eff r ()
restore config version =
  case config.source of
    AwsRdsSource {dbInstance} -> do
      let
        tempInstance = coerce dbInstance <> "-memento-temp"
        -- Rename current database
        renameRequest =
          newModifyDBInstance (coerce dbInstance)
            & modifyDBInstance_newDBInstanceIdentifier ?~ tempInstance
            & modifyDBInstance_deletionProtection ?~ False
        unrenameRequest =
          newModifyDBInstance tempInstance
            & modifyDBInstance_newDBInstanceIdentifier ?~ coerce dbInstance
        -- Create new database from renamed current
        restoreRequest =
          newRestoreDBInstanceToPointInTime (coerce dbInstance)
            & restoreDBInstanceToPointInTime_sourceDBInstanceIdentifier ?~ tempInstance
            & restoreDBInstanceToPointInTime_restoreTime ?~ version
        unrestoreRequest = newDeleteDBInstance (coerce dbInstance)

      retry (sendEither renameRequest) !!~ \_ -> void do
        log Warning "Rolling back renaming"
        retry (sendEither unrenameRequest)

      retry (sendEither restoreRequest) !!~ \_ -> void do
        log Warning "Removing rolledback database"
        retry (sendEither unrestoreRequest)

      pure ()
  where
    retry :: (Transaction Text :> r, Logger Message :> r, Concurrent :> r, Show e) => Eff r (Either e a) -> Eff r a
    retry = retryTx defRC (unlines . toList) . fmap (first show)

secondM :: (Bitraversable t, Applicative f) => (b -> f d) -> t c b -> f (t c d)
secondM = bitraverse pure
