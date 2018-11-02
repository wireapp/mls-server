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
    , reset

    -- * Cassandra-specific settings
    , CassandraSettings(..)
    , cassandraSchemaVersion
    ) where

import Imports
import Fmt
import Data.Aeson as Aeson
import Data.Aeson.Text as Aeson
import Control.Monad.Except
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import qualified Cassandra.Schema as C
import qualified Database.CQL.IO as C
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
    | CassandraStorage C.ClientState CassandraSettings

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
    { host :: Text          -- ^ Cassandra host
    , port :: Word16        -- ^ Cassandra port (usually 9042)
    , keyspace :: Text      -- ^ Keyspace to use for our data
    , tracing :: Bool       -- ^ Whether to enable tracing queries (the
                            --   traces can be seen in the
                            --   @system_traces.sessions@ table)
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
    c <- C.initialContactsPlain (host set)
    p <- C.init (Log.clone (Just "cassandra.mls-server") logger)
            $ C.setContacts (NE.head c) (NE.tail c)
            . C.setPortNumber (fromIntegral (port set))
            . C.setKeyspace (C.Keyspace (keyspace set))
            . C.setMaxConnections 4
            . C.setPoolStripes 4
            . C.setSendTimeout 3
            . C.setResponseTimeout 10
            . C.setProtocolVersion C.V3
            $ C.defSettings
    C.runClient p $ C.versionCheck cassandraSchemaVersion
    pure (CassandraStorage p set)

-- | Destroy the storage (close database connections, etc). Doesn't
-- guarantee that the storage can't be used after it's closed.
closeStorage :: Storage -> IO ()
closeStorage (InMemoryStorage _) =
    pure ()
closeStorage (CassandraStorage cas _) =
    C.shutdown cas

----------------------------------------------------------------------------
-- Storage methods
--
-- NB: These methods work for all storage options we support. Most of the
-- code inside the methods is storage-agnostic; any non-generic code resides
-- in helper functions.

-- | Get blobs stored for a specific group.
--
-- Allows getting all blobs, or only a range. Will fail if the range is
-- invalid.
getBlobs
    :: Storage
    -> GroupId         -- ^ Group ID
    -> Maybe Int64     -- ^ Beginning of the range (inclusive)
    -> Maybe Int64     -- ^ End of the range (exclusive)
    -> ExceptT MlsError IO [Blob]
getBlobs storage groupId mbFrom mbTo = do
    len <- liftIO $ getBlobCount storage groupId
    let from = fromMaybe 0 mbFrom
        to   = fromMaybe len mbTo
    unless (0 <= from && to <= len) $
        throwError $ BlobRangeOutOfBounds
            { allowedRange = (0, len)
            , requestedRange = (from, to) }
    unless (from <= to) $
        throwError $ InvalidBlobRange
            { requestedRange = (from, to) }
    -- NB: Here we rely on the fact that the list of blobs is append-only
    -- and the property of isolation is satisfied even if blobs get appended
    -- in the period of time between 'getBlobCount' and 'getRange'.
    liftIO $ getRange storage groupId (from, to)

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
    liftIO (maybeAppend storage groupId blob) >>= \case
        Right () -> pure ()
        Left expected ->
            throwError $ UnexpectedBlobIndex
                { expectedIndex = expected
                , gotIndex = blobIndex blob }

-- | Reset the storage.
reset
    :: Storage
    -> ExceptT MlsError IO ()
reset storage = case storage of
    InMemoryStorage var ->
        atomically $ StmMap.reset var
    CassandraStorage cas set -> do
        let q :: C.PrepQuery C.W () ()
            q = "truncate blobs"
        liftIO $ C.runClient cas $ C.write q $ params set ()

----------------------------------------------------------------------------
-- Helper functions

-- | Find out how many blobs are available.
getBlobCount
    :: Storage
    -> GroupId
    -> IO Int64
getBlobCount storage groupId = case storage of
    InMemoryStorage var -> atomically $
        maybe 0 genericLength <$> StmMap.lookup groupId var
    CassandraStorage cas set -> do
        let q :: C.PrepQuery C.R (Identity GroupId) (Identity Int64)
            q = "select count(*) from blobs \
                \where group = ?"
        fmap (maybe 0 runIdentity) $
            C.runClient cas $ C.query1 q $
            params set (Identity groupId)

-- | Get a range of blobs without doing any checks on it.
getRange
    :: Storage
    -> GroupId
    -> (Int64, Int64) -- ^ Range
    -> IO [Blob]
getRange storage groupId (from, to) = case storage of
    InMemoryStorage var -> atomically $
        genericTake (to-from) . genericDrop from . fromMaybe [] <$>
        StmMap.lookup groupId var
    CassandraStorage cas set -> do
        let q :: C.PrepQuery C.R (GroupId, Int64, Int64) (Int64, Text)
            q = "select index_, content from blobs \
                \where group = ? and index_ >= ? and index_ < ?"
        let mkBlob (i, c) = Blob
                { blobIndex = i
                , blobContent = case decodeFromText c of
                      Right x -> x
                      Left err -> error $ format
                          "Group {}, blob #{}: {}" groupId i err
                }
        fmap (map mkBlob) $
            C.runClient cas $ C.query q $
            params set (groupId, from, to)

-- | Either append the blob, or say what the index was expected to be.
maybeAppend
    :: Storage
    -> GroupId
    -> Blob
    -> IO (Either Int64 ())
maybeAppend storage groupId blob = case storage of
    InMemoryStorage var -> do
        let appendNew =
                if blobIndex blob == 0
                    then Focus.insert [blob] $> Right ()
                    else pure (Left 0)
        let appendExisting xs =
                let lastIndex = blobIndex (last xs) in
                if blobIndex blob == lastIndex + 1
                    then Focus.adjust (++ [blob]) $> Right ()
                    else pure (Left (lastIndex + 1))
        atomically $ StmMap.focus
            (Focus.lookup >>= maybe appendNew appendExisting)
            groupId var
    CassandraStorage cas set -> do
        len <- getBlobCount storage groupId
        let q :: C.PrepQuery C.W (GroupId, Int64, Text) C.Row
            q = "insert into blobs (group, index_, content) \
                \values (?, ?, ?) if not exists"
        let tryInsert = do
                [row] <-
                    C.runClient cas $ C.trans q $
                    params set (groupId,
                                blobIndex blob,
                                encodeToText (blobContent blob))
                -- The first column of the returned row signifies
                -- success/failure of the insert operation
                case C.fromRow 0 row of
                    Left err -> error err
                    Right Nothing -> error
                        "Expected [applied] to contain a value, got null"
                    Right (Just x) -> pure x
        if len /= blobIndex blob
            then pure (Left len)
            else tryInsert >>= \case
                     True  -> pure (Right ())
                     False -> Left <$> getBlobCount storage groupId

----------------------------------------------------------------------------
-- Utilities

-- | Encode to JSON.
encodeToText :: ToJSON a => a -> Text
encodeToText = TL.toStrict . Aeson.encodeToLazyText

-- | Decode from JSON.
decodeFromText :: FromJSON a => Text -> Either String a
decodeFromText = Aeson.eitherDecodeStrict . encodeUtf8

-- | Construct Cassandra params.
--
--   * Default consistency: 'C.Quorum'
--   * Query tracing: taken from 'CassandraSettings'
params :: C.Tuple a => CassandraSettings -> a -> C.QueryParams a
params set p = C.QueryParams
    { C.consistency = C.Quorum
    , C.skipMetaData = False
    , C.values = p
    , C.pageSize = Nothing
    , C.queryPagingState = Nothing
    , C.serialConsistency = Nothing
    , C.enableTracing = Just (tracing set)
    }
{-# INLINE params #-}
