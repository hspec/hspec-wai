{-# LANGUAGE OverloadedStrings #-}
module ExampleSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           Network.HTTP.Types (status200, hContentType)
import           Network.Wai (Application, responseLBS)

main :: IO ()
main = hspec spec

app :: Application
app _ = ($ responseLBS status200 [("Content-Type", "text/plain")] "hello")

run :: Application -> IO Application
run = return

spec :: Spec
spec = before (run app) $ do
  describe "GET /foo" $ do
    it "reponds with 200" $ do
      get "/foo" `shouldRespondWith` 200

    it "reponds with 'hello'" $ do
      get "/foo" `shouldRespondWith` "hello"

    it "reponds with 200 / 'hello'" $ do
      get "/foo" `shouldRespondWith` "hello" {matchStatus = 200}

    it "has Content-Type: text/plain" $ do
      get "/foo" `shouldHaveHeader` (hContentType, "text/plain")
