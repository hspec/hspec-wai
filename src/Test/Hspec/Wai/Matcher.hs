{-# LANGUAGE ViewPatterns #-}
module Test.Hspec.Wai.Matcher (
  ResponseMatcher(..)
, MatchHeader(..)
, (<:>)
, match
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.Maybe
import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types
import           Network.Wai.Test

import           Test.Hspec.Wai.Util

data ResponseMatcher = ResponseMatcher {
  matchStatus :: Int
, matchHeaders :: [MatchHeader]
, matchBody :: Maybe LB.ByteString
}

data MatchHeader = MatchHeader ([Header] -> Maybe String)

instance IsString ResponseMatcher where
  fromString s = ResponseMatcher 200 [] (Just . encodeUtf8 . fromString $ s)

instance Num ResponseMatcher where
  fromInteger n = ResponseMatcher (fromInteger n) [] Nothing
  (+) =    error "ResponseMatcher does not support (+)"
  (-) =    error "ResponseMatcher does not support (-)"
  (*) =    error "ResponseMatcher does not support (*)"
  abs =    error "ResponseMatcher does not support `abs`"
  signum = error "ResponseMatcher does not support `signum`"

match :: SResponse -> ResponseMatcher -> Maybe String
match (SResponse (Status status _) headers body) (ResponseMatcher expectedStatus expectedHeaders expectedBody) = mconcat [
    actualExpected "status mismatch:" (show status) (show expectedStatus) <$ guard (status /= expectedStatus)
  , checkHeaders headers expectedHeaders
  , expectedBody >>= matchBody_ body
  ]
  where
    matchBody_ (toStrict -> actual) (toStrict -> expected) = actualExpected "body mismatch:" actual_ expected_ <$ guard (actual /= expected)
      where
        (actual_, expected_) = case (safeToString actual, safeToString expected) of
          (Just x, Just y) -> (x, y)
          _ -> (show actual, show expected)

    actualExpected :: String -> String -> String -> String
    actualExpected message actual expected = unlines [
        message
      , "  expected: " ++ expected
      , "  but got:  " ++ actual
      ]

checkHeaders :: [Header] -> [MatchHeader] -> Maybe String
checkHeaders headers m = case go m of
    [] -> Nothing
    xs -> Just (mconcat xs ++ "the actual headers were:\n" ++ unlines (map formatHeader headers))
  where
    go = catMaybes . map (\(MatchHeader p) -> p headers)

(<:>) :: HeaderName -> ByteString -> MatchHeader
name <:> value = MatchHeader $ \headers -> guard (header `notElem` headers) >> (Just . unlines) [
    "missing header:"
  , formatHeader header
  ]
  where
    header = (name, value)
