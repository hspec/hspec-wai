module Test.Hspec.Wai.Matcher (
  ResponseMatcher(..)
, match
, haveHeader
) where


import           Control.Monad
import           Data.Monoid
import           Data.Functor
import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types
import           Network.Wai.Test

import           Test.Hspec.Wai.Util

data ResponseMatcher = ResponseMatcher {
  matchStatus :: Int
, matchHeaders :: [Header]
, matchBody :: Maybe LB.ByteString
}

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
    match_ "status mismatch" status expectedStatus
  , checkHeaders headers expectedHeaders
  , expectedBody >>= match_ "body mismatch" body
  ]
  where
    match_ :: (Show a, Eq a) => String -> a -> a -> Maybe String
    match_ message actual expected = actualExpected message actual expected <$ guard (actual /= expected)

    actualExpected :: Show a => String -> a -> a -> String
    actualExpected message actual expected = unlines [
        message
      , "  expected: " ++ show expected
      , "  but got:  " ++ show actual
      ]

checkHeaders :: [Header] -> [Header] -> Maybe String
checkHeaders actual expected = case filter (`notElem` actual) expected of
  [] -> Nothing
  missing ->
    let msg
          | length missing == 1 = "missing header"
          | otherwise = "missing headers"
    in Just $ unlines (msg : map (("  " ++) . formatHeader) missing)

haveHeader :: SResponse -> Header -> Maybe String
haveHeader (SResponse _ headers _) (name, expected) = go $ lookup name headers
  where
    go Nothing = Just $ "header doesn't exist: " ++ show name
    go (Just actual) = if actual == expected
                         then Nothing
                         else (Just . unlines) [ "header mismatch"
                                               , "  expected: \"" ++ B.unpack expected ++ "\""
                                               , "  but got:  \"" ++ B.unpack actual ++ "\""
                                               ]
