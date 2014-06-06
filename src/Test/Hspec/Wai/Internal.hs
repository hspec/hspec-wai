{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Test.Hspec.Wai.Internal (
  WaiExpectation
, WaiSession
, runWaiSession
, getApp
) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Network.Wai                (Application)
import           Network.Wai.Test           hiding (request)
import           Test.Hspec
import           Test.Hspec.Core            (Example (..))

type WaiExpectation = WaiSession ()

newtype WaiSession a = WaiSession {unWaiSession :: Session a}
  deriving (Functor, Applicative, Monad, MonadIO)

runWaiSession :: WaiSession a -> Application -> IO a
runWaiSession = runSession . unWaiSession

instance Example WaiExpectation where
  type Arg WaiExpectation = Application
  evaluateExample e p action = evaluateExample (action $ runWaiSession e) p ($ ())

getApp :: WaiSession Application
getApp = WaiSession ask
