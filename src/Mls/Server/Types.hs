-- | Types used throughout the project.
module Mls.Server.Types
    ( Blob(..)
    , GroupId
    ) where

import Imports
import Data.Aeson
import Data.Text

----------------------------------------------------------------------------
-- Blob

-- | The server stores an append-only list of blobs for each group; the
-- semantics of those blobs are at the discretion of the clients.
data Blob = Blob
    { blobIndex :: Int32    -- ^ A zero-based index of the blob in the append list
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

----------------------------------------------------------------------------
-- GroupId

-- | A group identifier (can be anything).
type GroupId = Text
