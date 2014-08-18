{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Network.HTTP.Types (hContentType)
import           Network.Wai (Application)

import           Control.Exception
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

appWithFixture :: ActionWith Application -> IO ()
appWithFixture action = withDatabaseConnection $ \c -> do
  loadFixture c
  app >>= action
  wipeDb c

loadFixture :: Connection -> IO ()
loadFixture _ = putStrLn "loaded fixture"

loadExtraFixture :: Connection -> IO ()
loadExtraFixture _ = putStrLn "loaded extra fixture"

wipeDb :: Connection -> IO ()
wipeDb _ = putStrLn "wiped all data from db"

data Connection = Connection

openConnection :: IO Connection
openConnection = return Connection

closeConnection :: Connection -> IO ()
closeConnection _ = return ()

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

withExtraFixture :: SpecWith Application -> SpecWith Application
withExtraFixture = beforeWith $ \app -> do
  withDatabaseConnection loadExtraFixture
  return app

spec :: Spec
spec = around appWithFixture $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with 'hello'" $ do
      get "/" `shouldRespondWith` "hello"

    withExtraFixture $ do
      it "responds with 200 / 'hello'" $ do
        get "/" `shouldRespondWith` "hello" {matchStatus = 200}

      it "has Content-Type: text/plain" $ do
        get "/" `shouldRespondWith` 200 {matchHeaders = [(hContentType, "text/plain")]}

  describe "GET /some-json" $ do
    it "responds with some JSON" $ do
      get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]
