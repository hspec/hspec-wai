{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Test.Hspec.Wai.JSON (
-- $setup
  json
, FromValue(..)
) where

import           Control.Arrow (second)
import           Data.List
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Aeson (Value, encode)
import           Data.Aeson.QQ
import qualified Data.CaseInsensitive as CI
import           Language.Haskell.TH.Quote

import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal (formatHeader)

-- $setup
-- The examples in this module assume that you have the @QuasiQuotes@ language
-- extension enabled and that "Data.ByteString.Lazy.Char8" is imported
-- qualified as @L@:
--
-- >>> :set -XQuasiQuotes
-- >>> import Data.ByteString.Lazy.Char8 as L

-- | A `QuasiQuoter` for constructing JSON values.
--
-- The constructed value is polymorph and unifies to instances of `FromValue`.
--
-- When used as a `ResponseMatcher` it matches a response with
--
--  * a status code of @200@
--
--  * a @Content-Type@ header with value @application/json@
--
--  * the specified JSON as response body
--
-- When used as a @ByteString@ it creates a ByteString from the specified JSON
-- that can be used as a request body for e.g. @POST@ and @PUT@ requests.
--
-- Example:
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
  fromValue v = ResponseMatcher 200 [MatchHeader p] (Just body) False
    where
      body = fromValue v

      permissibleHeaders = addIfASCII ("Content-Type", "application/json") [("Content-Type", "application/json; charset=utf-8")]

      addIfASCII h = if BL.all (< 128) body then (h :) else id

      mkCI = map (second CI.mk)

      p headers = if any (`elem` mkCI permissibleHeaders) (mkCI headers)
        then Nothing
        else (Just . unlines) ("missing header:" : (intersperse "  OR" $ map formatHeader permissibleHeaders))

instance FromValue ByteString where
  fromValue = encode

jsonPartial :: QuasiQuoter
jsonPartial = QuasiQuoter {
  quoteExp = \input -> [|fromPartialValue $(quoteExp aesonQQ input)|]
, quotePat = const $ error "No quotePat defined for Test.Hspec.Wai.JSON.jsonPartial"
, quoteType = const $ error "No quoteType defined for Test.Hspec.Wai.JSON.jsonPartial"
, quoteDec = const $ error "No quoteDec defined for Test.Hspec.Wai.JSON.jsonPartial"
}

class FromPartialValue a where
  fromPartialValue :: Value -> a

instance FromPartialValue ResponseMatcher where
  fromPartialValue v = ResponseMatcher 200 [MatchHeader p] (Just body) True
    where
      body = fromValue v

      permissibleHeaders = addIfASCII ("Content-Type", "application/json") [("Content-Type", "application/json; charset=utf-8")]

      addIfASCII h = if BL.all (< 128) body then (h :) else id

      mkCI = map (second CI.mk)

      p headers = if any (`elem` mkCI permissibleHeaders) (mkCI headers)
        then Nothing
        else (Just . unlines) ("missing header:" : (intersperse "  OR" $ map formatHeader permissibleHeaders))

instance FromPartialValue ByteString where
  fromPartialValue = encode
