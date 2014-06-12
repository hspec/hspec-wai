{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Wai.MatcherSpec (main, spec) where

import           Test.Hspec.Meta

import           Network.HTTP.Types
import           Network.Wai.Test

import           Test.Hspec.Wai.Matcher

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "match" $ do
    context "when both status and body do match" $ do
      it "returns Nothing" $ do
        SResponse status200 [] "" `match` 200
          `shouldBe` Nothing

    context "when status does not match" $ do
      it "returns an error message" $ do
        SResponse status404 [] "" `match` 200
          `shouldBe` (Just . unlines) [
            "status mismatch"
          , "  expected: 200"
          , "  but got:  404"
          ]

    context "when body does not match" $ do
      it "returns an error message" $ do
        SResponse status200 [] "foo" `match` "bar"
          `shouldBe` (Just . unlines) [
            "body mismatch"
          , "  expected: \"bar\""
          , "  but got:  \"foo\""
          ]

    context "when both status and body do not match" $ do
      it "combines error messages" $ do
        SResponse status404 [] "foo" `match` "bar"
          `shouldBe` (Just . unlines) [
            "status mismatch"
          , "  expected: 200"
          , "  but got:  404"
          , "body mismatch"
          , "  expected: \"bar\""
          , "  but got:  \"foo\""
          ]

    context "when matching headers" $ do
      context "when header is missing" $ do
        it "returns an error message" $ do
          SResponse status200 [] "" `match` 200 {matchHeaders = [("Content-Type", "application/json")]}
            `shouldBe` (Just . unlines) [
              "missing header"
            , "  Content-Type: application/json"
            ]

      context "when multiple headers are missing" $ do
        context "combines error messages" $ do
          it "returns an error message" $ do
            let expectedHeaders = [("Content-Type", "application/json"), ("Content-Encoding", "chunked")]
            SResponse status200 [(hContentLength, "23")] "" `match` 200 {matchHeaders = expectedHeaders}
              `shouldBe` (Just . unlines) [
                "missing headers"
              , "  Content-Type: application/json"
              , "  Content-Encoding: chunked"
              ]

  describe "haveHeader" $ do
    context "if expected header exists in actual response" $ do
      it "should be ok" $ do
        SResponse status200 [(hContentType, "text/plain" )] "" `haveHeader` (hContentType, "text/plain")
          `shouldBe` Nothing

    context "if expected header does not exist" $ do
      it "should be ok" $ do
        SResponse status200 [(hLocation, "http://example.com" )] "" `haveHeader` (hContentType, "text/plain")
          `shouldBe` Just ("header doesn't exist: " ++ show hContentType)

    context "if expected header has different value" $ do
      it "should be ok" $ do
        SResponse status200 [(hContentType, "image/jpeg" )] "" `haveHeader` (hContentType, "text/plain")
          `shouldBe` (Just . unlines) [
            "header mismatch"
          , "  expected: \"text/plain\""
          , "  but got:  \"image/jpeg\""
          ]
