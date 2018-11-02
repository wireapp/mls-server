module Main where

import Imports
import Control.Exception (bracket)

import Mls.Server
import Mls.Server.Util

main :: IO ()
main = do
    let desc = "MLS Server"
        defaultPath = "./conf/mls-server.yaml"
    set <- getOptions desc defaultPath
    bracket (newEnv set) closeEnv $ \env ->
        runServer env
