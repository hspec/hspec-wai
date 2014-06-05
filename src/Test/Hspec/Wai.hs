{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}
module Test.Hspec.Wai (
  WaiExpectation
, WaiSession
, runWaiSession
, get
, post
, put
, request
, shouldRespondWith
, ResponseMatcher(..)
) where

import           Control.Applicative
import           Data.Foldable

import           Test.Hspec

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

import           Network.HTTP.Types

import           Network.Wai.Test hiding (request)
import qualified Network.Wai.Test as Wai

import           Test.Hspec.Core (Example(..))
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Network.Wai (Application)

import           Network.Wai (Request(..))

import           Test.Hspec.Wai.Matcher

type WaiExpectation = WaiSession ()

newtype WaiSession a = WaiSession {unWaiSession :: Session a}
  deriving (Functor, Applicative, Monad, MonadIO)

runWaiSession :: WaiSession a -> Application -> IO a
runWaiSession = runSession . unWaiSession

instance Example WaiExpectation where
  type Arg WaiExpectation = Application
  evaluateExample e p action = evaluateExample (action $ runWaiSession e) p ($ ())

shouldRespondWith :: WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith action matcher = do
  r <- action
  forM_ (match r matcher) (liftIO . expectationFailure)

get :: ByteString -> WaiSession SResponse
get p = request methodGet p ""

post :: ByteString -> LB.ByteString -> WaiSession SResponse
post = request methodPost

put :: ByteString -> LB.ByteString -> WaiSession SResponse
put = request methodPut

request :: Method -> ByteString -> LB.ByteString -> WaiSession SResponse
request m p b = getApp >>= liftIO . runSession (Wai.srequest $ SRequest req b)
  where
    req = setPath defaultRequest {requestMethod = m} p

getApp :: WaiSession Application
getApp = WaiSession ask
