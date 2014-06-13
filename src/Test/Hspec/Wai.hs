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
, request

-- * Matching on the response
, shouldRespondWith
, ResponseMatcher(..)

-- * Matching on the response with predicates
, responseSatisfies
, statusSatisfies
, headersSatisfy
, bodySatisfies
, jsonBodySatisfies

-- * Helpers
, with
) where

import           Data.Aeson (decode, FromJSON)
import           Data.Foldable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Control.Applicative ((<$>))
import           Control.Monad (when)
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
-- > get "/" `shouldRespondWith` "foo" {matchHeaders = "Content-Type: text/plain"}
-- > -- matches if body is "foo", status is 200 and ther is a header field "Content-Type: text/plain"
shouldRespondWith :: WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith action matcher = do
  r <- action
  forM_ (match r matcher) (liftIO . expectationFailure)

-- | Check that the response satisfies the given predicate
responseSatisfies :: WaiSession SResponse
                  -> (SResponse -> Bool)
                  -> WaiExpectation
responseSatisfies action predicate = do
  resp <- action
  when (not $ predicate resp) $
    reportFailure $
      "expected the following response to match the predicate:\n"
        ++ show resp
        ++ "\nbut it didn't"

-- | Check that the body of the response satisfies the given predicate
bodySatisfies :: WaiSession SResponse 
              -> (LB.ByteString -> Bool)
              -> WaiExpectation
bodySatisfies action predicate = do
  body <- simpleBody <$> action
  when (not $ predicate body) $ 
    reportFailure
      "expected the body to match the predicate\nbut it didn't."

-- | Check that the 'Status' of the response satisfies the given predicate
statusSatisfies :: WaiSession SResponse
                -> (Status -> Bool)
                -> WaiExpectation
statusSatisfies action predicate = do
  status <- simpleStatus <$> action
  when (not $ predicate status) $ 
    reportFailure $
      "expected this status to match the predicate: " ++ show status
        ++ "\nbut it didn't" 

-- | Check that the 'Header's sastify the given predicate
headersSatisfy :: WaiSession SResponse
               -> ([Header] -> Bool)
               -> WaiExpectation
headersSatisfy action predicate = do
  headers <- simpleHeaders <$> action
  when (not $ predicate headers) $ 
    reportFailure $
      "expected these headers to match the predicate: " ++ show headers
        ++ "\nbut they didn't"

-- | Check that:
-- 
-- * the (JSON) body can successfully be decoded to the target type
-- * the value obtained that way matches the given predicate
jsonBodySatisfies :: (Show a, FromJSON a)
                  => WaiSession SResponse
                  -> (a -> Bool)
                  -> WaiExpectation
jsonBodySatisfies action predicate = do
  jsonBody <- simpleBody <$> action
  maybe (reportNothing jsonBody)
        (\value -> when (predicate value) $
                    reportPredicateFailure value
        )
        (decode jsonBody)

  where reportNothing body = reportFailure $ 
          "expected a valid JSON from this body:\n"
            ++ show body
            ++ "\nbut couldn't decode to the appropriate type"

        reportPredicateFailure value = reportFailure $ 
          "expected the predicate to succeed on: " 
            ++ show value
            ++ "\nbut it returned False"

-- | Perform a @GET@ request to the application under test.
get :: ByteString -> WaiSession SResponse
get path = request methodGet path ""

-- | Perform a @POST@ request to the application under test.
post :: ByteString -> LB.ByteString -> WaiSession SResponse
post = request methodPost

-- | Perform a @PUT@ request to the application under test.
put :: ByteString -> LB.ByteString -> WaiSession SResponse
put = request methodPut

-- | Perform a request to the application under test, with specified HTTP
-- method, request path and body.
request :: Method -> ByteString -> LB.ByteString -> WaiSession SResponse
request method path body = getApp >>= liftIO . runSession (Wai.srequest $ SRequest req body)
  where
    req = setPath defaultRequest {requestMethod = method} path

-- | Report a failure in hspec terms
reportFailure :: String -> WaiSession ()
reportFailure = liftIO . expectationFailure