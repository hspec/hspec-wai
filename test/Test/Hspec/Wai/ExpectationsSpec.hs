{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Wai.ExpectationsSpec (main, spec) where

import           Network.HTTP.Types (status200)
import           Network.Wai (Application, responseLBS)
import           Test.Hspec.Wai

main :: IO ()
main = hspec spec

app :: Application
app _ respond = respond $ responseLBS status200 [] ""

spec :: Spec
spec = with (return app) $ do
  describe "shouldRespondWith" $

    it "matches on a response" $
      get "/" `shouldRespondWith` 200

  describe "Test.Hspec re-exported expectations" $ do

    it "wraps shouldBe in a WaiSession" $
      True `shouldBe` True

    it "wraps shouldSatisfy in a WaiSession" $
      True `shouldSatisfy` const True

    it "wraps shouldReturn in a WaiSession" $
      return True `shouldReturn` True
