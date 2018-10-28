{-# LANGUAGE RecordWildCards #-}

-- | A prototype of an MLS server.
module Mls.Server
    (
    -- * Server
      runServer
    , app

    -- * Environment
    , Env(..)
    , newEnv
    , closeEnv
    ) where

import Imports
import Data.Aeson as Aeson
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Control.Monad.Except
import Control.Exception (bracket)
import qualified System.Logger as Log
import System.Logger (Logger)

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
    :> QueryParam "from" Int32
    :> QueryParam "to" Int32
    :> Get '[JSON] [Blob]
    :<|>
    -- Append a blob
       "groups" :> Capture "id" GroupId :> "blobs"
    :> ReqBody '[JSON] Blob
    :> PostNoContent '[JSON] NoContent  -- NB: 'PostNoContent' instructs
                                        -- servant to return code 204

-- | A list of handlers for the API.
server :: Env -> Server Api
server env =
    hoistServer (Proxy @Api) toHandler $
        Data.getBlobs (storage env)
        :<|>
        (\a b -> Data.appendBlob (storage env) a b $> NoContent)
  where
    toHandler :: ExceptT MlsError IO a -> Handler a
    toHandler = Handler . withExceptT mlsError

----------------------------------------------------------------------------
-- Server

-- | Run the server.
runServer :: Env -> IO ()
runServer env =
    run (fromIntegral (Settings.port (settings env))) (app env)

-- | Create a WAI 'Application'.
app :: Env -> Application
app env = serve (Proxy @Api) (server env)

----------------------------------------------------------------------------
-- Environment

-- | Environment with things like database connections, etc. Used by the
-- server.
data Env = Env
    { logger :: Logger
    , storage :: Storage
    , settings :: Settings
    }

-- | Create an environment according to some 'Settings'.
newEnv :: Settings -> IO Env
newEnv settings = do
    logger <- Log.new $ Log.defSettings
        & Log.setOutput Log.StdOut
        & Log.setFormat Nothing
    storage <- openStorage logger (Settings.storage settings)
    pure Env{..}

-- | Gracefully destroy the resources contained in the environment.
closeEnv :: Env -> IO ()
closeEnv env = do
    closeStorage (storage env)
    Log.flush (logger env) >> Log.close (logger env)
