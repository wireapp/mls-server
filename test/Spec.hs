{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Imports
import Control.Exception (bracket)
import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test hiding (request)
import Network.HTTP.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Mls.Server
import Mls.Server.Util

main :: IO ()
main = do
    let desc = "MLS Server Testsuite"
        defaultPath = "./conf/test/mls-server.yaml"
    set <- getOptions desc defaultPath
    bracket (newEnv set) closeEnv $ \env ->
        hspec (spec env)

spec :: Env -> Spec
spec env = with (pure (app env)) $ do

    -- Reset the storage before we start
    describe "POST /i/reset" $ do
        it "resets the storage" $ do
            post "/i/reset" "" `shouldRespondWith` 204
            get "/groups/1/blobs" `shouldRespondWith`
                [json| [] |]

    -- Get blobs in range [X; Y)
    describe "GET /groups/:id/blobs" $ do
        it "succeeds for a non-existent group" $
            -- No group = empty list
            get "/groups/1/blobs" `shouldRespondWith`
                [json| [] |]

        it "gets a range of blobs" $ do
            -- Generate three blobs with different types of content
            let blob0 = [json| {"index": 0, "content": "Hello"} |]
                blob1 = [json| {"index": 1, "content": 256} |]
                blob2 = [json| {"index": 2, "content": true} |]
            -- Post all of them
            postJson "/groups/1/blobs" blob0 `shouldRespondWith` 204
            postJson "/groups/1/blobs" blob1 `shouldRespondWith` 204
            postJson "/groups/1/blobs" blob2 `shouldRespondWith` 204
            -- Try to get blobs 0,1
            get "/groups/1/blobs?from=0&to=2" `shouldRespondWith`
                [json| [ {"index": 0, "content": "Hello"}
                       , {"index": 1, "content": 256} ] |]

        it "allows omitting the range" $ do
            -- Without the range, all blobs should be returned
            get "/groups/1/blobs" `shouldRespondWith`
                [json| [ {"index": 0, "content": "Hello"}
                       , {"index": 1, "content": 256}
                       , {"index": 2, "content": true} ] |]

        it "allows empty ranges" $ do
            get "/groups/1/blobs?from=2&to=2" `shouldRespondWith`
                [json| [] |]

        it "fails on ranges where from>to" $ do
            get "/groups/1/blobs?from=2&to=1" `shouldRespondWith` 400

        it "fails on out-of-bounds ranges" $ do
            get "/groups/1/blobs?from=1&to=4" `shouldRespondWith` 400
            get "/groups/1/blobs?from=-1&to=0" `shouldRespondWith` 400

    -- Append a blob (and check the counter)
    describe "POST /groups/:id/blobs" $ do
        it "doesn't accept blobs from the past" $ do
            -- A blob with the same index as the current one won't be accepted
            let blob2 = [json| {"index": 2, "content": false} |]
            postJson "/groups/1/blobs" blob2 `shouldRespondWith` 400
            -- The contents should not be rewritten
            get "/groups/1/blobs?from=2" `shouldRespondWith`
                [json| [ {"index": 2, "content": true} ] |]

        it "doesn't accept blobs from the future" $ do
            -- A blob with an index that doesn't immediately follow the
            -- current one won't be accepted
            let blob4 = [json| {"index": 4, "content": false} |]
            postJson "/groups/1/blobs" blob4 `shouldRespondWith` 400
            -- Check the full contents just in case
            get "/groups/1/blobs" `shouldRespondWith`
                [json| [ {"index": 0, "content": "Hello"}
                       , {"index": 1, "content": 256}
                       , {"index": 2, "content": true} ] |]

        it "doesn't allow starting a new group with index != 0" $ do
            -- For a new group, a blob with a non-zero index won't be accepted
            let blob1 = [json| {"index": 1, "content": 256} |]
            postJson "/groups/2/blobs" blob1 `shouldRespondWith` 400
            -- A blob with a negative index won't be accepted either
            let blob_1 = [json| {"index": -1, "content": 256} |]
            postJson "/groups/2/blobs" blob_1 `shouldRespondWith` 400
            -- No group = empty list
            get "/groups/2/blobs" `shouldRespondWith`
                [json| [] |]

----------------------------------------------------------------------------
-- Helpers

-- | Like 'post', but sets the proper content type. Without that, servant
-- rejects requests with error code 415.
postJson :: ByteString -> BSL.ByteString -> WaiSession SResponse
postJson path = request methodPost path [(hContentType, "application/json")]
