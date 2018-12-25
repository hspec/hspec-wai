{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Test.Hspec.Wai.UtilSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.Hspec.Wai.Util
import           Network.HTTP.Types (parseSimpleQuery)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

#if !MIN_VERSION_base(4,12,0)
import           Data.Monoid
#endif

main :: IO ()
main = hspec spec

decodePair :: (ByteString, ByteString) -> (String, String)
decodePair (a, b) = (decode a, decode b)
  where
    decode :: ByteString -> String
    decode = newlineNormalize . T.unpack . T.decodeUtf8

    newlineNormalize :: String -> String
    newlineNormalize input = case input of
      [] -> []
      '\r' : '\n' : xs -> '\n': newlineNormalize xs
      x : xs -> x : newlineNormalize xs

spec :: Spec
spec = do
  describe "formUrlEncodeQuery" $ do
    it "separates keys from values by =" $ do
      formUrlEncodeQuery [("foo", "bar")] `shouldBe` "foo=bar"

    it "separates pairs by &" $ do
      formUrlEncodeQuery [("foo", "bar"), ("foo", "baz")] `shouldBe` "foo=bar&foo=baz"

    it "applies newline normalization" $ do
      formUrlEncodeQuery [("text", "foo\nbar\nbaz\n")] `shouldBe` "text=foo%0D%0Abar%0D%0Abaz%0D%0A"

    it "handles Unicode characters" $ do
      formUrlEncodeQuery [("foo","bar-\955-baz")] `shouldBe` "foo=bar-%CE%BB-baz"

    context "when encoding characters in the printable ASCII range" $ do
      let input = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
          reference = "+%21%22%23%24%25%26%27%28%29*%2B%2C-.%2F0123456789%3A%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D%7E"

      it "behaves like Firefox" $ do
        formUrlEncodeQuery [("foo", input)] `shouldBe` "foo=" <> reference

    modifyMaxSize (`div` 5) $ do
      it "can be reverted by Network.HTTP.Types.parseSimpleQuery" $ do
        property $ \xs -> do
          (map decodePair . parseSimpleQuery . LB.toStrict . formUrlEncodeQuery) xs `shouldBe` xs

  describe "safeToString" $ do
    context "when used on an empty string" $ do
      it "returns Nothing" $ do
        safeToString "" `shouldBe` Nothing

  describe "formatHeader" $ do
    it "formats header" $ do
      let header = ("Content-Type", "application/json")
      formatHeader header `shouldBe` "  Content-Type: application/json"

    describe "when ends with whitespace" $ do
      it "uses show" $ do
        let header = ("Content-Type", "application/json  ")
        formatHeader header `shouldBe` "  " ++ show header

    describe "when contains non-print characters" $ do
      it "uses show" $ do
        let header = ("Content-\nType", "application/json")
        formatHeader header `shouldBe` "  " ++ show header

    describe "when header is not decodable as UTF-8" $ do
      it "uses show" $ do
        let header = ("Content-Type", "\xc3\x28")
        formatHeader header `shouldBe` "  " ++ show header
