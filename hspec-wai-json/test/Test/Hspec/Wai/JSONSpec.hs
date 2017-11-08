{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Test.Hspec.Wai.JSONSpec (main, spec) where

import           Test.Hspec
import           Data.String

import           Test.Hspec.Wai hiding (pending)
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "json" $ do
    context "when matching body" $ do
      let MatchBody matcher = matchBody [json|{foo: 23}|]

      it "ignores whitespace" $ do
        let
          actual = fromString $ unlines [
              "{"
            , show ("foo" :: String) ++ " : 23"
            , "}"
            ]
        matcher [] actual `shouldBe` Nothing

      it "rejects bodies that are not equal" $ do
        matcher [] [json|{foo: 42}|] `shouldBe` Just (unlines [
            "body mismatch:"
          , "  expected: {\"foo\":23}"
          , "  but got:  {\"foo\":42}"
          ])

    context "when matching Content-Type header" $ do
      let
        body = [json|{foo: 23}|]
        [MatchHeader matcher] = matchHeaders body
        match = (`matcher` body)

      it "accepts 'application/json'" $ do
        match [("Content-Type", "application/json")] `shouldBe` Nothing

      it "ignores 'charset=utf-8'" $ do
        match [("Content-Type", "application/json;charset=utf-8")] `shouldBe` Nothing

      it "ignores whitespace" $ do
        match [("Content-Type", "application/json ; charset=utf-8")] `shouldBe` Nothing

      it "requires a Content-Type header" $ do
        match [] `shouldBe` (Just. unlines) [
            "missing header:"
          , "  Content-Type: application/json"
          ]

      it "rejects other values for Content-Type" $ do
        match [("Content-Type", "foobar")] `shouldBe` (Just . unlines) [
            "missing header:"
          , "  Content-Type: application/json"
          ]
