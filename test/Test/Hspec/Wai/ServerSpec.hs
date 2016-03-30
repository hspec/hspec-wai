{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Wai.ServerSpec where

import           Control.Exception
import           Network.HTTP.Types
import           Network.Wai
import           System.IO.Silently
import           System.Process
import           Test.Hspec

import           Test.Hspec.Wai.Server

spec :: Spec
spec = do
  describe "withApplication" $ do
    it "runs a wai Application while executing the given action" $ do
      let mkApp = return $ \ _request respond -> respond $ responseLBS ok200 [] "foo"
      withApplication mkApp $ \ port -> do
        output <- silence $ readProcess "curl" ["localhost:" ++ show port] ""
        output `shouldBe` "foo"

    it "propagates exceptions from the server to the executing thread" $ do
      let mkApp = return $ \ _request _respond -> throwIO $ ErrorCall "foo"
      (withApplication mkApp $ \ port -> do
          silence $ readProcess "curl" ["localhost:" ++ show port] "")
        `shouldThrow` (== ErrorCall "foo")
