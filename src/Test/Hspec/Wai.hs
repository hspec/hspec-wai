{-# LANGUAGE OverloadedStrings #-}
-- | Have a look at the <https://github.com/hspec/hspec-wai#readme README> for
-- an example of how to use this library.
module Test.Hspec.Wai (
-- * Types
  WaiSession
, WaiExpectation

-- * Performing requests
, get
, post
, put
, patch
, delete
, request

-- ** Performing requests with encoded parameters
, postWithParams

-- * Matching on the response
, shouldRespondWith
, ResponseMatcher(..)
, MatchHeader(..)
, (<:>)

-- * Helpers and re-exports
, liftIO
, with
, Hspec.Spec
, Hspec.hspec
, Hspec.describe
, Hspec.context
, Hspec.it
, Hspec.specify
, Test.Hspec.Wai.pending
, Test.Hspec.Wai.pendingWith

-- ** Re-exported Test.Hspec expectations
, shouldBe
, shouldSatisfy
, shouldReturn
, shouldThrow
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Control.Monad.IO.Class
import           Network.Wai (Request(..))
import           Network.HTTP.Types
import           Network.Wai.Test hiding (request)
import qualified Network.Wai.Test as Wai
import qualified Test.Hspec as Hspec

import           Test.Hspec.Wai.Internal
import           Test.Hspec.Wai.Matcher
import           Test.Hspec.Wai.Expectation

-- | An alias for `before`.
with :: IO a -> Hspec.SpecWith a -> Hspec.Spec
with = Hspec.before

-- | A lifted version of `Test.Hspec.pending`.
pending :: WaiSession ()
pending = liftIO Hspec.pending

-- | A lifted version of `Test.Hspec.pendingWith`.
pendingWith :: String -> WaiSession ()
pendingWith = liftIO . Hspec.pendingWith


-- | Perform a @GET@ request to the application under test.
get :: ByteString -> WaiSession SResponse
get path = request methodGet path [] ""

-- | Perform a @POST@ request to the application under test.
post :: ByteString -> LB.ByteString -> WaiSession SResponse
post path = request methodPost path []

-- | Perform a @PUT@ request to the application under test.
put :: ByteString -> LB.ByteString -> WaiSession SResponse
put path = request methodPut path []

-- | Perform a @PATCH@ request to the application under test.
patch :: ByteString -> LB.ByteString -> WaiSession SResponse
patch path = request methodPatch path []

-- | Perform a @DELETE@ request to the application under test.
delete :: ByteString -> WaiSession SResponse
delete path = request methodDelete path [] ""

-- | Perform a request to the application under test, with specified HTTP
-- method, request path, headers and body.
request :: Method -> ByteString -> [Header] -> LB.ByteString -> WaiSession SResponse
request method path headers body = getApp >>= liftIO . runSession (Wai.srequest $ SRequest req body)
  where
    req = setPath defaultRequest {requestMethod = method, requestHeaders = headers} path

-- | Perform a @POST@ request to the application under test with a list of
-- key-value tuples to be encoded as query parameters
postWithParams :: ByteString -> SimpleQuery -> WaiSession SResponse
postWithParams path = request methodPost path formHeaders . encodeParams
  where
    encodeParams = LB.fromStrict . renderSimpleQuery False
    formHeaders  = [(hContentType, "application/x-www-form-urlencoded")]
