{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Wai.Internal (
  WaiExpectation
, WaiSession(..)
, runWaiSession
, getApp
, formatHeader
) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Network.Wai (Application)
import           Network.Wai.Test hiding (request)
import           Test.Hspec
import           Test.Hspec.Core (Example (..))
import           Test.Hspec.Wai.Util (formatHeader)

-- | An expectation in the `WaiSession` monad.  Failing expectations are
-- communicated through exceptions (similar to `Expectation` and
-- `Test.HUnit.Base.Assertion`).
type WaiExpectation = WaiSession ()

-- | A <http://www.yesodweb.com/book/web-application-interface WAI> test
-- session that carries the `Application` under test an some client state.
newtype WaiSession a = WaiSession {unWaiSession :: Session a}
  deriving (Functor, Applicative, Monad, MonadIO)

runWaiSession :: WaiSession a -> Application -> IO a
runWaiSession = runSession . unWaiSession

instance Example WaiExpectation where
  type Arg WaiExpectation = Application
  evaluateExample e p action = evaluateExample (action $ runWaiSession e) p ($ ())

getApp :: WaiSession Application
getApp = WaiSession ask
