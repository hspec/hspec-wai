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
    it "" $ do
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
