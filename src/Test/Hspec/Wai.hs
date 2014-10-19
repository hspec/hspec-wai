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

-- * Matching on the response
, shouldRespondWith
, ResponseMatcher(..)
, MatchHeader(..)
, (<:>)

-- * Run test session
, withApplication

-- * Helpers and re-exports
, liftIO
) where

import           Data.Foldable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Control.Monad.IO.Class
import           Network.Wai (Request(..))
import           Network.HTTP.Types
import           Network.Wai.Test hiding (request)
import qualified Network.Wai.Test as Wai
import           Test.Hspec.Expectations

import           Test.Hspec.Wai.Internal
import           Test.Hspec.Wai.Matcher

import           Network.Wai (Application)

-- | Run test with specified `Application`.
withApplication :: IO Application -> WaiExpectation -> Expectation
withApplication app action = app >>= runWaiSession action

-- | Set the expectation that a response matches a specified `ResponseMatcher`.
--
-- A @ResponseMatcher@ matches a response if:
--
-- * the specified status matches the HTTP response status code
--
-- * the specified body (if any) matches the response body
--
-- * the response has all of the specified `Header` fields
--   (the response may have arbitrary additional `Header` fields)
--
-- You can use @ResponseMatcher@'s (broken) `Num` instance to match for a HTTP
-- status code:
--
-- > get "/" `shouldRespondWith` 200
-- > -- matches if status is 200
--
-- You can use @ResponseMatcher@'s `IsString` instance to match for a HTTP
-- status @200@ and a body:
--
-- > get "/" `shouldRespondWith` "foo"
-- > -- matches if body is "foo" and status is 200
--
-- If you want to match for a different HTTP status, you can use record update
-- notation to specify `matchStatus` explicitly:
--
-- > get "/" `shouldRespondWith` "foo" {matchStatus = 404}
-- > -- matches if body is "foo" and status is 404
--
-- If you want to require a specific header field you can specify
-- `matchHeaders`:
--
-- > get "/" `shouldRespondWith` "foo" {matchHeaders = ["Content-Type" <:> "text/plain"]}
-- > -- matches if body is "foo", status is 200 and ther is a header field "Content-Type: text/plain"
shouldRespondWith :: WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith action matcher = do
  r <- action
  forM_ (match r matcher) (liftIO . expectationFailure)

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
