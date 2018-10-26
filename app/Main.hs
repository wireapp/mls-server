module Main where

import Imports

import Mls.Server
import Mls.Server.Util

main :: IO ()
main = do
    let desc = "MLS Server"
        defaultPath = "/etc/wire/mls-server/conf/mls-server.yaml"
    options <- getOptions desc Nothing defaultPath
    runServer options
