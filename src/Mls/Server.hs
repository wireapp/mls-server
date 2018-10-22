-- | A prototype of an MLS server.
module Mls.Server
    ( startMlsServer
    , mlsServer
    ) where

import BasePrelude hiding (Handler)
import Fmt
import Data.Aeson
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
    if 0 <= from && from <= to && to <= len
       then pure (take (to-from) (drop from allBlobs))
       else throwError $ err400 { errBody = format
                "Requested range is [{}; {}), which is not inside [{}; {})"
                from to (0 :: Int) len }

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
        Left expectedIndex -> throwError $ err400 { errBody =
            "The inserted blob should have index "+|expectedIndex|+", "<>
            "but its index is "+|blobIndex blob|+"" }
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
