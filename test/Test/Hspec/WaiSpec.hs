{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.WaiSpec (main, spec) where

import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types
import Network.Wai

import Test.Hspec.Wai

main :: IO ()
main = hspec spec

expectMethod :: Method -> IO Application
expectMethod method = return $ \req respond -> do
  requestMethod req `shouldBe` method
  respond $ responseLBS status200 [] ""

expectRequest :: Method -> ByteString -> ByteString -> [Header] -> Application
expectRequest method path body headers req respond = do
  requestMethod req `shouldBe` method
  rawPathInfo req `shouldBe` path
  requestHeaders req `shouldBe` headers
  rawBody <- requestBody req
  rawBody `shouldBe` body
  respond $ responseLBS status200 [] ""

spec :: Spec
spec = do
  describe "get" $ do
    it "sends a get request" $ withApplication (expectMethod methodGet) $ do
      get "/" `shouldRespondWith` 200

  describe "post" $ do
    it "sends a post request" $ withApplication (expectMethod methodPost) $ do
      post "/" "" `shouldRespondWith` 200

  describe "put" $ do
    it "sends a put request" $ withApplication (expectMethod methodPut) $ do
      put "/" "" `shouldRespondWith` 200

  describe "delete" $ do
    it "sends a delete request" $ withApplication (expectMethod methodDelete) $ do
      delete "/" `shouldRespondWith` 200

  describe "request" $ do
    it "sends method, path, headers, and body" $ withApplication (return $ expectRequest methodGet "/foo" body accept) $ do
      request methodGet "/foo" accept (BL.fromChunks [body]) `shouldRespondWith` 200

  where
    accept = [(hAccept, "application/json")]
    body = "{\"foo\": 1}"
