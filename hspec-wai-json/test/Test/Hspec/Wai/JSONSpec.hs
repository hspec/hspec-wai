{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
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
      let [MatchHeader matcher] = matchHeaders [json|{foo: 23}|]

      context "when body is ASCII" $ do
        let
          body = [json|{foo: 23}|]
          match = (`matcher` body)

        it "accepts 'application/json'" $ do
          match [("Content-Type", "application/json")] `shouldBe` Nothing

        it "accepts 'application/json;charset=utf-8'" $ do
          match [("Content-Type", "application/json;charset=utf-8")]
            `shouldBe` Nothing

        it "rejects other headers" $ do
          match [("Content-Type", "foobar")] `shouldBe` (Just . unlines) [
              "missing header:"
            , "  Content-Type: application/json"
            ]
