{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Wai.UtilSpec (main, spec) where

import           Test.Hspec.Meta

import           Test.Hspec.Wai.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatHeader" $ do
    it "formats header" $ do
      let header = ("Content-Type", "application/json")
      formatHeader header `shouldBe` "Content-Type: application/json"

    describe "when starts with whitespace" $ do
      it "uses show" $ do
        let header = ("  Content-Type", "application/json")
        formatHeader header `shouldBe` show header

    describe "when ends with whitespace" $ do
      it "uses show" $ do
        let header = ("Content-Type", "application/json  ")
        formatHeader header `shouldBe` show header

    describe "when contains non-print characters" $ do
      it "uses show" $ do
        let header = ("Content-\nType", "application/json")
        formatHeader header `shouldBe` show header

    describe "when header is not decodable as UTF-8" $ do
      it "uses show" $ do
        let header = ("Content-Type", "\xc3\x28")
        formatHeader header `shouldBe` show header
