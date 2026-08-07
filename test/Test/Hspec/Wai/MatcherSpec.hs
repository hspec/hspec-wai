{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Wai.MatcherSpec (main, spec) where

import           Test.Hspec

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
            "status mismatch:"
          , "  expected: 200"
          , "  but got:  404"
          ]

    context "when body does not match" $ do
      it "returns an error message" $ do
        SResponse status200 [] "foo" `match` "bar"
          `shouldBe` (Just . unlines) [
            "body mismatch:"
          , "  expected: bar"
          , "  but got:  foo"
          ]

      context "when one body contains unsafe characters" $ do
        it "uses show for both bodies in the error message" $ do
          SResponse status200 [] "foo\nbar" `match` "bar"
            `shouldBe` (Just . unlines) [
              "body mismatch:"
            , "  expected: \"bar\""
            , "  but got:  \"foo\\nbar\""
            ]

    context "when both status and body do not match" $ do
      it "combines error messages" $ do
        SResponse status404 [] "foo" `match` "bar"
          `shouldBe` (Just . unlines) [
            "status mismatch:"
          , "  expected: 200"
          , "  but got:  404"
          , "body mismatch:"
          , "  expected: bar"
          , "  but got:  foo"
          ]

    context "when matching headers" $ do
      context "when header is missing" $ do
        it "returns an error message" $ do
          SResponse status200 [] "" `match` 200 {matchHeaders = ["Content-Type" <:> "application/json"]}
            `shouldBe` (Just . unlines) [
              "missing header:"
            , "  Content-Type: application/json"
            , "the actual headers were:"
            ]

      context "when multiple headers are missing" $ do
        it  "combines error messages" $ do
          let expectedHeaders = ["Content-Type" <:> "application/json", "Content-Encoding" <:> "chunked"]
          SResponse status200 [(hContentLength, "23")] "" `match` 200 {matchHeaders = expectedHeaders}
            `shouldBe` (Just . unlines) [
              "missing header:"
            , "  Content-Type: application/json"
            , "missing header:"
            , "  Content-Encoding: chunked"
            , "the actual headers were:"
            , "  Content-Length: 23"
            ]

    context "when body has a partial match" $ do
      it "returns Nothing" $ do
        SResponse status200 [] "@=.#" `match` 200 { matchBody = bodyContains "=." }
          `shouldBe` Nothing

    context "when body has NO partial match" $ do
      it "returns an error message" $ do
        SResponse status200 [] "@.#" `match` 200 { matchBody = bodyContains "=." }
          `shouldBe` (Just . unlines) [
            "body substring search failed:"
          , "  expected: =."
          , "  but got:  @.#"
          ]
