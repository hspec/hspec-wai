{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Test.Hspec.Wai.JSON (
-- $setup
  json
, FromValue(..)
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Aeson (Value, decode, encode)
import           Data.Aeson.QQ
import           Language.Haskell.TH.Quote

import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

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
  fromValue = ResponseMatcher 200 [matchHeader] . equalsJSON
    where
      matchHeader = MatchHeader $ \headers _body ->
        case lookup "Content-Type" headers of
          Just h | isJSON h -> Nothing
          _ -> Just $ unlines [
              "missing header:"
            , formatHeader ("Content-Type", "application/json")
            ]
      isJSON c = media == "application/json" && parameters `elem` ignoredParameters
        where
          (media, parameters) = let (m, p) = breakAt ';' c in (strip m, strip p)

          -- Technically, no parameters are required nor optional for
          -- application/json.  However, as charset=utf-8 is widely added by
          -- other software and compliant recipients should ignore any charset
          -- (as per http://www.iana.org/assignments/media-types/application/json)
          -- we ignore charset=utf-8 here.
          --
          -- This is a decision made for pragmatism!
          --
          -- I'm still pretty much against ignoring any other charsets.  Adding
          -- a charset parameter is non-standard and hspec-wai is not just a
          -- compliant recipients but a testing software.  Hence I take the
          -- stance that the job of a testing software is not just to accept
          -- what a compliant client would accept but also to enforce standard
          -- conformance.
          ignoredParameters = ["", "charset=utf-8"]

      breakAt c = fmap (B.drop 1) . B.break (== c)
      strip = B.reverse . B.dropWhile isSpace . B.reverse . B.dropWhile isSpace

equalsJSON :: Value -> MatchBody
equalsJSON expected = MatchBody matcher
  where
    matcher headers actualBody = case decode actualBody of
      Just actual | actual == expected -> Nothing
      _ -> let MatchBody m = bodyEquals (encode expected) in m headers actualBody

instance FromValue ByteString where
  fromValue = encode
