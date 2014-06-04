hspec-wai [![Build Status](https://travis-ci.org/hspec/hspec-wai.svg?branch=master)](https://travis-ci.org/hspec/hspec-wai)
===========

Helpers to test [WAI](http://www.yesodweb.com/book/web-application-interface) application with [Hspec](http://hspec.github.io/)

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ExampleSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           Network.HTTP.Types (status200)
import           Network.Wai        (Application, responseLBS)

app :: Application
app _ = return $ responseLBS status200 [("Content-Type", "text/plain")] "hello"

run :: Application -> IO Application
run = return

spec :: Spec
spec = before (run app) $ do
  describe "GET /foo" $ do
    it "reponds with 200" $ do
      get "/foo" `shouldRespondWith` 200

    it "reponds with 'hello'" $ do
      get "/foo" `shouldRespondWith` "hello"

    it "reponds with 200 / 'bar'" $ do
      get "/foo" `shouldRespondWith` "hello" {matchStatus = 200}
```

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
