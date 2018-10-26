{-# LANGUAGE OverloadedStrings #-}

module Main where

import Imports
import Control.Exception (finally)
import Cassandra.Schema
import System.Logger hiding (info)

import Mls.Server.Util

import qualified V0

main :: IO ()
main = do
    let desc = "MLS Server Schema Migrations"
        defaultPath = "/etc/wire/mls-server/conf/mls-server-schema.yaml"
    o <- getOptions desc Nothing defaultPath
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V0.migration
        ] `finally` close l
        -- When adding new migrations here, don't forget to update
        -- 'cassandraSchemaVersion' in Mls.Server
