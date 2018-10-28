{-# LANGUAGE NamedFieldPuns #-}

-- | Error-handling.
module Mls.Server.Error
    ( MlsError(..)
    , mlsError
    ) where

import Imports
import Fmt
import Servant
import Data.Aeson as Aeson

-- | All errors that can be thrown by API handlers.
data MlsError
    = BlobRangeOutOfBounds
          { allowedRange :: (Int32, Int32)
          , requestedRange :: (Int32, Int32) }
    | InvalidBlobRange
          { requestedRange :: (Int32, Int32) }
    | UnexpectedBlobIndex
          { expectedIndex :: Int32
          , gotIndex :: Int32 }
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
