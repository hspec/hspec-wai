{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.WaiSpec (main, spec) where

import qualified Test.Hspec as Hspec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types
import Network.Wai

import Test.Hspec.Wai

main :: IO ()
main = Hspec.hspec spec

expectMethod :: Method -> Application
expectMethod method req respond = do
  requestMethod req `Hspec.shouldBe` method
  respond $ responseLBS status200 [] ""

expectRequest :: Method -> ByteString -> ByteString -> [Header] -> Application
expectRequest method path body headers req respond = do
  requestMethod req `Hspec.shouldBe` method
  rawPathInfo req `Hspec.shouldBe` path
  requestHeaders req `Hspec.shouldBe` headers
  rawBody <- requestBody req
  rawBody `Hspec.shouldBe` body
  respond $ responseLBS status200 [] ""

spec :: Spec
spec = do
  describe "get" $ with (return $ expectMethod methodGet) $
    it "sends a get request" $
      get "/" `shouldRespondWith` 200

  describe "post" $ with (return $ expectMethod methodPost) $
    it "sends a post request" $
      post "/" "" `shouldRespondWith` 200

  describe "put" $ with (return $ expectMethod methodPut) $
    it "sends a put request" $
      put "/" "" `shouldRespondWith` 200

  describe "delete" $ with (return $ expectMethod methodDelete) $
    it "sends a delete request" $
      delete "/" `shouldRespondWith` 200

  describe "request" $ with (return $ expectRequest methodGet "/foo" jsonBody jsonAccept) $
    it "sends method, path, headers, and body" $
      request methodGet "/foo" jsonAccept (BL.fromChunks [jsonBody]) `shouldRespondWith` 200

  describe "postQuery" $ with (return $ expectRequest methodPost "/foo" formBody formEncoded) $
    it "sends a post request with form-encoded params" $
      postQuery "/foo" queryParams `shouldRespondWith` 200

  where
    jsonAccept = [(hAccept, "application/json")]
    jsonBody = "{\"foo\": 1}"
    formEncoded = [(hContentType, "application/x-www-form-urlencoded")]
    formBody = "foo=1"
    queryParams = [("foo", "1")]
