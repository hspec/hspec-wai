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
, postWithParams'
, putWithParams
, putWithParams'
, patchWithParams
, patchWithParams'

-- * Matching on the response
, shouldRespondWith
, ResponseMatcher(..)
, MatchHeader(..)
, (<:>)

-- * Helpers and re-exports
, liftIO
, with
, Test.Hspec.Wai.pending
, Test.Hspec.Wai.pendingWith
) where

import           Data.Foldable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Control.Monad.IO.Class
import           Network.Wai (Request(..))
import           Network.HTTP.Types
import           Network.Wai.Test hiding (request)
import qualified Network.Wai.Test as Wai
import           Test.Hspec

import           Test.Hspec.Wai.Internal
import           Test.Hspec.Wai.Matcher

-- | An alias for `before`.
with :: IO a -> SpecWith a -> Spec
with = before

-- | A lifted version of `Test.Hspec.pending`.
pending :: WaiSession ()
pending = liftIO Test.Hspec.pending

-- | A lifted version of `Test.Hspec.pendingWith`.
pendingWith :: String -> WaiSession ()
pendingWith = liftIO . Test.Hspec.pendingWith

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

-- | Perform a @POST@ request to the application under test with a list of
-- parameters to be encoded, including empty parameters.
postWithParams :: ByteString -> Query -> WaiSession SResponse
postWithParams = requestWithParams methodPost

-- | Perform a @POST@ request to the application under test with a list of
-- non-empty parameters to be encoded.
postWithParams' :: ByteString -> SimpleQuery -> WaiSession SResponse
postWithParams' = requestWithParams' methodPost

-- | Perform a @PUT@ request to the application under test with a list of
-- parameters to be encoded, including empty parameters.
putWithParams :: ByteString -> Query -> WaiSession SResponse
putWithParams = requestWithParams methodPut

-- | Perform a @PUT@ request to the application under test with a list of
-- non-empty parameters to be encoded.
putWithParams' :: ByteString -> SimpleQuery -> WaiSession SResponse
putWithParams' = requestWithParams' methodPut

-- | Perform a @PATCH@ request to the application under test with a list of
-- parameters to be encoded, including empty parameters.
patchWithParams :: ByteString -> Query -> WaiSession SResponse
patchWithParams = requestWithParams methodPatch

-- | Perform a @PATCH@ request to the application under test with a list of
-- non-empty parameters to be encoded.
patchWithParams' :: ByteString -> SimpleQuery -> WaiSession SResponse
patchWithParams' = requestWithParams' methodPatch

requestWithParams :: Method -> ByteString -> Query -> WaiSession SResponse
requestWithParams method path = request method path formEncoded . encodeParams
  where
   encodeParams = LB.fromStrict . renderQuery False

requestWithParams' :: Method -> ByteString -> SimpleQuery -> WaiSession SResponse
requestWithParams' method path = request method path formEncoded . encodeParams
  where
   encodeParams = LB.fromStrict . renderSimpleQuery False

formEncoded :: [Header]
formEncoded = [(hContentType, "application/x-www-form-urlencoded")]
