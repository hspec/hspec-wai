{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.WaiSpec (main, spec) where

import           Test.Hspec
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Types
import           Network.Wai

import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal (withApplication)

main :: IO ()
main = hspec spec

expectMethod :: Method -> Application
expectMethod method req respond = do
  requestMethod req `shouldBe` method
  respond $ responseLBS status200 [] ""

expectRequest :: Method -> ByteString -> ByteString -> [Header] -> Application
expectRequest method path body headers req respond = do
  requestMethod req `shouldBe` method
  rawPathInfo req `shouldBe` path
  requestHeaders req `shouldBe` headers
  rawBody <- getRequestBodyChunk req
  rawBody `shouldBe` body
  respond $ responseLBS status200 [] ""

spec :: Spec
spec = do
  describe "WaiSession" $ do
    it "has a MonadFail instance" $ do
      withApplication undefined $ do
        23 <- return (42 :: Int)
        return ()
      `shouldThrow` anyIOException

  describe "get" $ with (return $ expectMethod methodGet) $ do
    it "sends a get request" $ do
      get "/" `shouldRespondWith` 200

  describe "post" $ with (return $ expectMethod methodPost) $ do
    it "sends a post request" $ do
      post "/" "" `shouldRespondWith` 200

  describe "put" $ with (return $ expectMethod methodPut) $ do
    it "sends a put request" $ do
      put "/" "" `shouldRespondWith` 200

  describe "options" $ with (return $ expectMethod methodOptions) $ do
    it "sends an options request" $ do
      options "/" `shouldRespondWith` 200

  describe "delete" $ with (return $ expectMethod methodDelete) $ do
    it "sends a delete request" $ do
      delete "/" `shouldRespondWith` 200

  describe "request" $ with (return $ expectRequest methodGet "/foo" body accept) $ do
    it "sends method, path, headers, and body" $ do
      request methodGet "/foo" accept (BL.fromChunks [body]) `shouldRespondWith` 200

  describe "postHtmlForm" $ with (return $ expectRequest methodPost "/foo" "foo=bar" formEncoded) $ do
    it "sends a post request with form-encoded params" $ do
      postHtmlForm "/foo" [("foo", "bar")] `shouldRespondWith` 200

  where
    accept = [(hAccept, "application/json")]
    body = "{\"foo\": 1}"
    formEncoded = [(hContentType, "application/x-www-form-urlencoded")]
