hspec-wai [![Build Status](https://travis-ci.org/hspec/hspec-wai.svg?branch=master)](https://travis-ci.org/hspec/hspec-wai)
===========

Helpers to test [WAI](http://www.yesodweb.com/book/web-application-interface)
applications with [Hspec](http://hspec.github.io/)

## Example

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai

import           Network.HTTP.Types (status200, hContentType)
import           Network.Wai (Application, responseLBS)

main :: IO ()
main = hspec spec

app :: Application
app _ = ($ responseLBS status200 [("Content-Type", "text/plain")] "hello")

spec :: Spec
spec = before (return app) $ do
  describe "GET /foo" $ do
    it "reponds with 200" $ do
      get "/foo" `shouldRespondWith` 200

    it "reponds with 'hello'" $ do
      get "/foo" `shouldRespondWith` "hello"

    it "reponds with 200 / 'hello'" $ do
      get "/foo" `shouldRespondWith` "hello" {matchStatus = 200}

    it "has Content-Type: text/plain" $ do
      get "/foo" `shouldRespondWith` 200 {matchHeaders = [(hContentType, "text/plain")]}
~~~

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
