{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V0 (migration) where

import Imports
import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 0 "Initial schema" $ do
    void $ schema' [r|
        CREATE TABLE if not exists blobs
            ( group        text
            , index_       int
            , content      text    -- JSON
            , primary key  (group, index_)
            ) with clustering order by (index_ asc);
        |]
