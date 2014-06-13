hspec-wai [![Build Status](https://travis-ci.org/hspec/hspec-wai.svg?branch=master)](https://travis-ci.org/hspec/hspec-wai)
===========

Helpers to test [WAI](http://www.yesodweb.com/book/web-application-interface)
applications with [Hspec](http://hspec.github.io/)

## Example

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Network.HTTP.Types (hContentType)
import           Network.Wai (Application)

import           Data.Aeson (Value(..), object, (.=))
import qualified Web.Scotty as S

main :: IO ()
main = hspec spec

app :: IO Application
app = S.scottyApp $ do
  S.get "/" $ do
    S.text "hello"

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "reponds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "reponds with 'hello'" $ do
      get "/" `shouldRespondWith` "hello"

    it "reponds with 200 / 'hello'" $ do
      get "/" `shouldRespondWith` "hello" {matchStatus = 200}

    it "has Content-Type: text/plain" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = [(hContentType, "text/plain")]}

  describe "GET /some-json" $ do
    it "reponds with some JSON" $ do
      get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]
~~~

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
