{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Test.Hspec.Wai.JSONSpec (main, spec) where

import           Test.Hspec

import           Test.Hspec.Wai hiding (shouldBe)
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "json" $ do
    context "when matching Content-Type header" $ do
      context "when JSON is ASCII" $ do
        let [MatchHeader matcher] = matchHeaders [json|{foo: 23}|]
        it "accepts 'application/json'" $ do
          matcher [("Content-Type", "application/json")] `shouldBe` Nothing

        it "accepts 'application/json; charset=utf-8'" $ do
          matcher [("Content-Type", "application/json; charset=utf-8")] `shouldBe` Nothing

        it "rejects other headers" $ do
          matcher [("Content-Type", "foobar")] `shouldBe` (Just . unlines) [
              "missing header:"
            , "  Content-Type: application/json"
            , "  OR"
            , "  Content-Type: application/json; charset=utf-8"
            ]

      context "when JSON contains Unicode" $ do
        let [MatchHeader matcher] = matchHeaders [json|{foo: #{"\955" :: String}}|]
        it "rejects 'application/json'" $ do
          matcher [("Content-Type", "application/json")] `shouldBe` (Just . unlines) [
              "missing header:"
            , "  Content-Type: application/json; charset=utf-8"
            ]

        it "accepts 'application/json; charset=utf-8'" $ do
          matcher [("Content-Type", "application/json; charset=utf-8")] `shouldBe` Nothing
