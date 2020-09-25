{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Test.Hspec.Wai.Internal (
  WaiExpectation
, WaiSession(..)
, runWaiSession
, runWithState
, withApplication
, getApp
, getState
, formatHeader
) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Network.Wai (Application)
import           Network.Wai.Test hiding (request)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Wai.Util (formatHeader)

#if MIN_VERSION_base(4,9,0)
import           Control.Monad.Fail
#endif

-- | An expectation in the `WaiSession` monad.  Failing expectations are
-- communicated through exceptions (similar to `Test.Hspec.Expectations.Expectation` and
-- `Test.HUnit.Base.Assertion`).
type WaiExpectation st = WaiSession st ()

-- | A <http://www.yesodweb.com/book/web-application-interface WAI> test
-- session that carries the `Application` under test and some client state.
newtype WaiSession st a = WaiSession {unWaiSession :: ReaderT st Session a}
  deriving (Functor, Applicative, Monad, MonadIO
#if MIN_VERSION_base(4,9,0)
  , MonadFail
#endif
  )

runWaiSession :: WaiSession () a -> Application -> IO a
runWaiSession action app = runWithState action ((), app)

runWithState :: WaiSession st a -> (st, Application) -> IO a
runWithState action (st, app) = runSession (flip runReaderT st $ unWaiSession action) app

withApplication :: Application -> WaiSession () a -> IO a
withApplication = flip runWaiSession

instance Example (WaiExpectation st) where
  type Arg (WaiExpectation st) = (st, Application)
  evaluateExample e p action = evaluateExample (action $ runWithState e) p ($ ())

getApp :: WaiSession st Application
getApp = WaiSession (lift ask)

getState :: WaiSession st st
getState = WaiSession ask
