{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Wai.Internal (
  WaiExpectation
, WaiSession(..)
, RequestAction
, runWaiSession
, formatHeader
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types
import           Network.Wai.Test hiding (request)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Wai.Util (formatHeader)

-- | An expectation in the `WaiSession` monad.  Failing expectations are
-- communicated through exceptions (similar to `Expectation` and
-- `Test.HUnit.Base.Assertion`).
type WaiExpectation = WaiSession ()

-- | A <http://www.yesodweb.com/book/web-application-interface WAI> test
-- session that carries the `Application` under test an some client state.
newtype WaiSession a = WaiSession {unWaiSession :: ReaderT RequestAction IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

type RequestAction = Method -> ByteString -> [Header] -> LB.ByteString -> IO SResponse

runWaiSession :: WaiSession a -> RequestAction -> IO a
runWaiSession = runReaderT . unWaiSession

instance Example WaiExpectation where
  type Arg WaiExpectation = RequestAction
  evaluateExample e p action = evaluateExample (action $ runWaiSession e) p ($ ())
