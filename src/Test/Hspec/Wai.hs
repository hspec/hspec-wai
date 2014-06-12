{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}
module Test.Hspec.Wai (
  with
, get
, post
, put
, request
, shouldRespondWith
, ResponseMatcher(..)
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

with :: IO a -> SpecWith a -> Spec
with = before

-- |
-- Passs if
--
--   * the given number matches with the HTTP Status code of the response.
--   * the given string matches with the body of the response.
--   * the given `ResponseMatcher` matches with the response.
--
-- Example:
--
-- > get "/foo" `shouldRespondWith` 200                         -- Pass
-- > get "/foo" `shouldRespondWith` "bar"                       -- Pass if the body is "bar"
-- > get "/foo" `shouldRespondWith` "bar" { matchStatus = 200 } -- Pass if the body is "bar" and status is 200
--

shouldRespondWith :: WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith action matcher = do
  r <- action
  forM_ (match r matcher) (liftIO . expectationFailure)

-- | Performs `GET` request to running app.
get :: ByteString -> WaiSession SResponse
get p = request methodGet p ""

-- | Performs `POST` request to running app.
post :: ByteString -> LB.ByteString -> WaiSession SResponse
post = request methodPost

-- | Performs `PUT` request to running app.
put :: ByteString -> LB.ByteString -> WaiSession SResponse
put = request methodPut

-- | Performs request to running app, with HTTP Method, path and body.
request :: Method -> ByteString -> LB.ByteString -> WaiSession SResponse
request m p b = getApp >>= liftIO . runSession (Wai.srequest $ SRequest req b)
  where
    req = setPath defaultRequest {requestMethod = m} p
