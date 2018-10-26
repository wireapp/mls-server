-- | Storage for the server and operations on it.
module Mls.Server.Data
    (
    -- * A unified storage API
      Storage(..)
    , StorageSettings(..)
    -- ** Init/destroy
    , openStorage
    , closeStorage
    -- ** Methods
    , getBlobs
    , appendBlob

    -- * Cassandra-specific settings
    , CassandraSettings(..)
    , cassandraSchemaVersion
    ) where

import Imports
import Data.Aeson
import Control.Monad.Except
import qualified Cassandra as Cas
import qualified Cassandra.Settings as Cas
import qualified Cassandra.Schema as Cas
import qualified StmContainers.Map as StmMap
import qualified Focus
import qualified Data.List.NonEmpty as NE
import qualified System.Logger as Log

import Mls.Server.Types
import Mls.Server.Error

----------------------------------------------------------------------------
-- Storage settings

-- | Runtime data needed for storage (either a database connection or an
-- in-memory variable).
data Storage
    -- | Store everything in memory, no persistence.
    = InMemoryStorage (StmMap.Map GroupId [Blob])
    -- | Store data in Cassandra.
    | CassandraStorage Cas.ClientState

-- | Storage settings.
data StorageSettings
    -- | Store everything in memory, no persistence
    = UseInMemory
    -- | Store everything in Cassandra
    | UseCassandra CassandraSettings
    deriving (Eq, Show, Generic)

instance FromJSON StorageSettings

----------------------------------------------------------------------------
-- Cassandra

-- | Settings needed to be able to talk to Cassandra.
data CassandraSettings = CassandraSettings
    { host :: Text
    , port :: Word16
    , keyspace :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON CassandraSettings

-- | Our schema version.
--
-- Should be in sync with the @mls-server-schema@ executable.
cassandraSchemaVersion :: Int32
cassandraSchemaVersion = 0

----------------------------------------------------------------------------
-- Storage init/destroy

-- | Initialize storage (set up database connections, etc).
openStorage :: Log.Logger -> StorageSettings -> IO Storage
openStorage _ UseInMemory =
    InMemoryStorage <$> StmMap.newIO
openStorage logger (UseCassandra set) = do
    c <- Cas.initialContactsPlain (host set)
    p <- Cas.init (Log.clone (Just "cassandra.brig") logger)
            $ Cas.setContacts (NE.head c) (NE.tail c)
            . Cas.setPortNumber (fromIntegral (port set))
            . Cas.setKeyspace (Cas.Keyspace (keyspace set))
            . Cas.setMaxConnections 4
            . Cas.setPoolStripes 4
            . Cas.setSendTimeout 3
            . Cas.setResponseTimeout 10
            . Cas.setProtocolVersion Cas.V3
            $ Cas.defSettings
    Cas.runClient p $ Cas.versionCheck cassandraSchemaVersion
    pure (CassandraStorage p)

-- | Destroy the storage (close database connections, etc). Doesn't
-- guarantee that the storage can't be used after it's closed.
closeStorage :: Storage -> IO ()
closeStorage (InMemoryStorage _) =
    pure ()
closeStorage (CassandraStorage cas) =
    Cas.shutdown cas

----------------------------------------------------------------------------
-- Storage methods
--
-- NB: These methods work for all storage options we support. Most of the
-- code inside the methods is storage-agnostic; any non-generic code resides
-- in the helper functions (in 'where').

-- | Get blobs stored for a specific group.
--
-- Allows getting all blobs, or only a range. Will fail if the range is
-- invalid.
getBlobs
    :: Storage
    -> GroupId         -- ^ Group ID
    -> Maybe Int       -- ^ Beginning of the range (inclusive)
    -> Maybe Int       -- ^ End of the range (exclusive)
    -> ExceptT MlsError IO [Blob]
getBlobs storage groupId mbFrom mbTo = do
    allBlobs <- liftIO getAllBlobs
    let len  = length allBlobs
        from = fromMaybe 0 mbFrom
        to   = fromMaybe len mbTo
    unless (0 <= from && to <= len) $
        throwError $ BlobRangeOutOfBounds
            { allowedRange = (0, len)
            , requestedRange = (from, to) }
    unless (from <= to) $
        throwError $ InvalidBlobRange
            { requestedRange = (from, to) }
    pure (take (to-from) (drop from allBlobs))
  where
    -- Get all blobs as a list.
    getAllBlobs :: IO [Blob]
    getAllBlobs = case storage of
        InMemoryStorage var -> atomically $
            fromMaybe [] <$> StmMap.lookup groupId var
        CassandraStorage cas ->
            undefined

-- | Append a single blob to the group-stored blobs.
--
-- Also works if the group has no blobs stored for it yet. Will fail if the
-- index in the blob doesn't directly follow the previous index.
appendBlob
    :: Storage
    -> GroupId         -- ^ Group ID
    -> Blob            -- ^ Blob to append
    -> ExceptT MlsError IO ()
appendBlob storage groupId blob = do
    liftIO append >>= \case
        Right () -> pure ()
        Left expectedIndex ->
            throwError $ UnexpectedBlobIndex
                { expectedIndex = expectedIndex
                , gotIndex = blobIndex blob }
  where
    -- Either append the blob, or say what the index was expected to be.
    append :: IO (Either Int ())
    append = case storage of
        InMemoryStorage var ->
            let update = Focus.lookup >>= \case
                    Nothing ->
                        if blobIndex blob == 0
                            then Focus.insert [blob] $> Right ()
                            else pure (Left 0)
                    Just xs ->
                        let lastIndex = blobIndex (last xs) in
                        if blobIndex blob == lastIndex + 1
                            then Focus.adjust (++ [blob]) $> Right ()
                            else pure (Left (lastIndex + 1))
            in atomically $ StmMap.focus update groupId var
        CassandraStorage cas ->
            undefined
