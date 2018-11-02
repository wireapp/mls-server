-- | Server config.
module Mls.Server.Settings
    ( Settings(..)
    , StorageSettings(..)
    ) where

import Imports
import Data.Aeson

import Mls.Server.Data (StorageSettings(..))

-- | Settings that are consumed on startup and that can be accessed at
-- runtime.
data Settings = Settings
    { storage :: StorageSettings        -- ^ Where the server stores stuff
    , port :: Word16                    -- ^ On what port the server should run
    } deriving (Eq, Show, Generic)

instance FromJSON Settings
