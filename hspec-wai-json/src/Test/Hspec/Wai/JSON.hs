{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Test.Hspec.Wai.JSON (
-- $setup
  json
, FromValue(..)
) where

import           Test.Hspec.Wai
import           Data.ByteString.Lazy (ByteString)
import           Data.Aeson (Value, encode)
import           Data.Aeson.QQ
import           Language.Haskell.TH.Quote

-- $setup
-- The examples in this module assume that you have the @QuasiQuotes@ language
-- extension enabled and that "Data.ByteString.Lazy.Char8" is imported
-- qualified as @L@:
--
-- >>> :set -XQuasiQuotes
-- >>> import Data.ByteString.Lazy.Char8 as L

-- | A `QuasiQuoter` for constructing JSON values.
--
-- The constructed value is polymorph and unifies to instances of `FromValue`
-- (either `ByteString` or `ResponseMatcher`).
--
-- Example
--
-- >>> L.putStrLn [json|[23, {foo: 42}]|]
-- [23,{"foo":42}]
json :: QuasiQuoter
json = QuasiQuoter {
  quoteExp = \input -> [|fromValue $(quoteExp aesonQQ input)|]
, quotePat = const $ error "No quotePat defined for Test.Hspec.Wai.JSON.json"
, quoteType = const $ error "No quoteType defined for Test.Hspec.Wai.JSON.json"
, quoteDec = const $ error "No quoteDec defined for Test.Hspec.Wai.JSON.json"
}

class FromValue a where
  fromValue :: Value -> a

instance FromValue ResponseMatcher where
  fromValue v = ResponseMatcher 200 [("Content-Type", "application/json")] (Just . encode $ v)

instance FromValue ByteString where
  fromValue = encode
