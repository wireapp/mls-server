{-# LANGUAGE NamedFieldPuns #-}

-- | A prototype of an MLS server.
module Mls.Server
    ( startMlsServer
    , mlsServer
    ) where

import BasePrelude hiding (Handler)
import Fmt
import Data.Aeson as Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Text (Text)
import Servant
import qualified StmContainers.Map as StmMap
import qualified Focus

----------------------------------------------------------------------------
-- Types

-- | The server stores an append-only list of blobs for each group; the
-- semantics of those blobs are at the discretion of the clients.
data Blob = Blob
    { blobIndex :: Int      -- ^ A zero-based index of the blob in the append list
    , blobContent :: Value  -- ^ Blob contents (any JSON value)
    } deriving (Eq, Show, Generic)

instance ToJSON Blob where
    toJSON o = object
        [ "index" .= blobIndex o
        , "content" .= blobContent o
        ]

instance FromJSON Blob where
    parseJSON = withObject "Blob" $ \o ->
        Blob <$> o .: "index"
             <*> o .: "content"

-- | A group identifier (can be anything).
type GroupId = Text

----------------------------------------------------------------------------
-- Error-handling

-- | All errors that can be thrown by API handlers.
data MlsError
    = BlobRangeOutOfBounds
          { allowedRange :: (Int, Int)
          , requestedRange :: (Int, Int) }
    | InvalidBlobRange
          { requestedRange :: (Int, Int) }
    | UnexpectedBlobIndex
          { expectedIndex :: Int
          , gotIndex :: Int }
    deriving (Eq, Show)

-- | Render a 'MlsError' as a 'ServantErr'.
mlsError :: MlsError -> ServantErr
mlsError = \case
    BlobRangeOutOfBounds {allowedRange, requestedRange} ->
        let description =
                "Requested range is "+|rangeF requestedRange|+
                ", which is not inside "+|rangeF allowedRange|+""
        in  mkError err400 "BlobRangeOutOfBounds" description $ object
                [ "allowed_range" .= range allowedRange
                , "requested_range" .= range requestedRange ]

    InvalidBlobRange {requestedRange} ->
        let description =
                "The lower end of requested range "+|rangeF requestedRange|+
                " is higher than the upper end"
        in mkError err400 "InvalidBlobRange" description $ object
            [ "requested_range" .= range requestedRange ]

    UnexpectedBlobIndex {expectedIndex, gotIndex} ->
        let description =
                "The new blob should have index "+|expectedIndex|+
                ", but got a blob with index "+|gotIndex|+""
        in mkError err400 "UnexpectedBlobIndex" description $ object
            [ "expected_index" .= expectedIndex
            , "got_index" .= gotIndex ]

  where
    -- Create a Servant error
    mkError :: ServantErr -> Text -> Text -> Aeson.Value -> ServantErr
    mkError err tag description body = err {
        errBody = Aeson.encode $ object
            [ "tag" .= tag
            , "description" .= description
            , "body" .= body ] }
    -- Format a range as JSON
    range (from, to) = object ["from" .= from, "to" .= to]
    -- Format a range as text
    rangeF (from, to) = format "[{}; {})" from to :: Builder

----------------------------------------------------------------------------
-- API

-- | A type-level spec of the server API.
type Api =
    -- Get blobs
       "groups" :> Capture "id" GroupId :> "blobs"
    :> QueryParam "from" Int
    :> QueryParam "to" Int
    :> Get '[JSON] [Blob]
    :<|>
    -- Append a blob
       "groups" :> Capture "id" GroupId :> "blobs"
    :> ReqBody '[JSON] Blob
    :> PostNoContent '[JSON] NoContent  -- NB: 'PostNoContent' instructs
                                        -- servant to return code 204

-- | A list of handlers for the API.
server :: Server Api
server =
    getBlobs
    :<|>
    appendBlob

----------------------------------------------------------------------------
-- Implementation

-- | A global variable serving as in-memory storage.
storage :: StmMap.Map GroupId [Blob]
storage = unsafePerformIO StmMap.newIO
{-# NOINLINE storage #-}

-- | Get blobs stored for a specific group.
--
-- Allows getting all blobs, or only a range. Will fail if the range is
-- invalid.
getBlobs
    :: GroupId         -- ^ Group ID
    -> Maybe Int       -- ^ Beginning of the range (inclusive)
    -> Maybe Int       -- ^ End of the range (exclusive)
    -> Handler [Blob]
getBlobs groupId mbFrom mbTo = do
    allBlobs <- liftIO $ atomically $
                fromMaybe [] <$> StmMap.lookup groupId storage
    let len  = length allBlobs
        from = fromMaybe 0 mbFrom
        to   = fromMaybe len mbTo
    unless (0 <= from && to <= len) $
        throwError $ mlsError $ BlobRangeOutOfBounds
            { allowedRange = (0, len)
            , requestedRange = (from, to) }
    unless (from <= to) $
        throwError $ mlsError $ InvalidBlobRange
            { requestedRange = (from, to) }
    pure (take (to-from) (drop from allBlobs))

-- | Append a single blob to the group-stored blobs.
--
-- Also works if the group has no blobs stored for it yet. Will fail if the
-- index in the blob doesn't directly follow the previous index.
appendBlob
    :: GroupId         -- ^ Group ID
    -> Blob            -- ^ Blob to append
    -> Handler NoContent
appendBlob groupId blob = do
    liftIO (atomically (StmMap.focus update groupId storage)) >>= \case
        Right () -> pure NoContent
        Left expectedIndex -> throwError $
            mlsError $ UnexpectedBlobIndex
                { expectedIndex = expectedIndex
                , gotIndex = blobIndex blob }
  where
    -- Either append or say what the index was expected to be
    update :: Focus.Focus [Blob] STM (Either Int ())
    update = Focus.lookup >>= \case
        Nothing ->
            if blobIndex blob == 0
                then Focus.insert [blob] $> Right ()
                else pure (Left 0)
        Just xs ->
            let lastIndex = blobIndex (last xs) in
            if blobIndex blob == lastIndex + 1
                then Focus.adjust (++ [blob]) $> Right ()
                else pure (Left (lastIndex + 1))

----------------------------------------------------------------------------
-- Wrappers

-- | Run an MLS server on port 8080.
startMlsServer :: IO ()
startMlsServer = run 8080 mlsServer

-- | A WAI 'Application' for the server, suitable for running on any port or
-- embedding into a larger application.
mlsServer :: Application
mlsServer = serve (Proxy @Api) server
