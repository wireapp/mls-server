-- | A prototype of an MLS server.
module Mls.Server
    ( runServer
    , inMemoryServer
    ) where

import Imports
import Data.Aeson as Aeson
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Control.Monad.Except
import Control.Exception (bracket)
import qualified System.Logger as Log

import Mls.Server.Data as Data
import Mls.Server.Error
import Mls.Server.Types
import qualified Mls.Server.Settings as Settings
import Mls.Server.Settings (Settings)

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
server :: Storage -> Server Api
server storage =
    hoistServer (Proxy @Api) toHandler $
        Data.getBlobs storage
        :<|>
        (\a b -> Data.appendBlob storage a b $> NoContent)
  where
    toHandler :: ExceptT MlsError IO a -> Handler a
    toHandler = Handler . withExceptT mlsError

----------------------------------------------------------------------------
-- Wrappers

-- | Run the server.
runServer :: Settings -> IO ()
runServer settings = do
    bracket newEnv closeEnv $ \(_logger, storage) ->
        run (fromIntegral (Settings.port settings)) $
            serve (Proxy @Api) (server storage)
  where
    newEnv = do
        logger <- Log.new $ Log.defSettings
            & Log.setOutput Log.StdOut
            & Log.setFormat Nothing
        storage <- openStorage logger (Settings.storage settings)
        pure (logger, storage)

    closeEnv (logger, storage) = do
        closeStorage storage
        Log.flush logger >> Log.close logger

-- | Create a test application that only works in-memory.
inMemoryServer :: IO Application
inMemoryServer = do
    logger <- Log.new $ Log.defSettings
        & Log.setOutput Log.StdOut
        & Log.setFormat Nothing
    storage <- openStorage logger UseInMemory
    pure $ serve (Proxy @Api) (server storage)
