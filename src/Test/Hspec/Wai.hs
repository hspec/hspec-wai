{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
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
, options
, delete
, request

-- ** Posting HTML forms
  -- *** URL-encoded
, postHtmlForm
, postUrlEncodedForm
-- *** Files
, postMultipartForm
, FileMeta(..)

-- * Matching on the response
, shouldRespondWith

, ResponseMatcher(..)
, MatchHeader(..)
, MatchBody(..)
, Body
, (<:>)

-- * Helpers and re-exports
, liftIO
, with
, withState
, getState
, pending
, pendingWith
) where

import           Prelude ()
import "base-compat" Prelude.Compat

import           Data.Foldable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Network.Wai (Request(..))
import           Network.HTTP.Types
import           Network.Wai.Test hiding (request)
import qualified Network.Wai.Test as Wai
import           Test.Hspec.Expectations

import           Test.Hspec.Core.Spec hiding (pending, pendingWith)
import qualified Test.Hspec.Core.Spec as Core
import           Test.Hspec.Core.Hooks

import           Test.Hspec.Wai.Util
import           Test.Hspec.Wai.Internal
import           Test.Hspec.Wai.Matcher

import           Network.Wai (Application)

with :: IO Application -> SpecWith ((), Application) -> Spec
with action = before ((,) () <$> action)

withState :: IO (st, Application) -> SpecWith (st, Application) -> Spec
withState = before

-- | A lifted version of `Core.pending`.
pending :: WaiSession st ()
pending = liftIO Core.pending

-- | A lifted version of `Core.pendingWith`.
pendingWith :: String -> WaiSession st ()
pendingWith = liftIO . Core.pendingWith

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
-- > -- matches if body is "foo", status is 200 and there is a header field "Content-Type: text/plain"
shouldRespondWith :: HasCallStack => WaiSession st SResponse -> ResponseMatcher -> WaiExpectation st
shouldRespondWith action matcher = do
  r <- action
  forM_ (match r matcher) (liftIO . expectationFailure)

-- | Perform a @GET@ request to the application under test.
get :: ByteString -> WaiSession st SResponse
get path = request methodGet path [] ""

-- | Perform a @POST@ request to the application under test.
post :: ByteString -> LB.ByteString -> WaiSession st SResponse
post path = request methodPost path []

-- | Perform a @PUT@ request to the application under test.
put :: ByteString -> LB.ByteString -> WaiSession st SResponse
put path = request methodPut path []

-- | Perform a @PATCH@ request to the application under test.
patch :: ByteString -> LB.ByteString -> WaiSession st SResponse
patch path = request methodPatch path []

-- | Perform an @OPTIONS@ request to the application under test.
options :: ByteString -> WaiSession st SResponse
options path = request methodOptions path [] ""

-- | Perform a @DELETE@ request to the application under test.
delete :: ByteString -> WaiSession st SResponse
delete path = request methodDelete path [] ""

-- | Perform a request to the application under test, with specified HTTP
-- method, request path, headers and body.
request :: Method -> ByteString -> [Header] -> LB.ByteString -> WaiSession st SResponse
request method path headers = WaiSession . lift . Wai.srequest . SRequest req
  where
    req = setPath defaultRequest {requestMethod = method, requestHeaders = headers} path

-- | Perform a @POST@ request to the application under test.
--
-- The specified list of key-value pairs is encoded as
-- @application/x-www-form-urlencoded@ and used as request body.
--
-- In addition the @Content-Type@ is set to @application/x-www-form-urlencoded@.
postHtmlForm :: ByteString -- ^ path
             -> [(String, String)] -> WaiSession st SResponse
postHtmlForm path = request methodPost path [(hContentType, "application/x-www-form-urlencoded")] . formUrlEncodeQuery

-- | Synonym for 'postHtmlForm'
postUrlEncodedForm :: ByteString -- ^ path
                   -> [(String, String)] -> WaiSession st SResponse
postUrlEncodedForm = postHtmlForm

-- | @POST@ a @multipart/form-data@ form which might include files.
--
-- The @Content-Type@ is set to @multipart/form-data; boundary=<bd>@ where @bd@ is the part separator without the @--@ prefix.
postMultipartForm :: ByteString -- ^ path
                  -> ByteString -- ^ part separator
                  -> [(FileMeta, ByteString, ByteString, ByteString)] -- ^ (file metadata, field MIME type, field name, field contents)
                  -> WaiSession st SResponse
postMultipartForm path sbs =
  request methodPost path [(hContentType, "multipart/form-data; boundary=" <> sbs)] . formMultipartQuery sbs
