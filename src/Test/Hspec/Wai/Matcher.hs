module Test.Hspec.Wai.Matcher where

import           Control.Monad
import           Data.Monoid
import           Data.Functor
import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types
import           Network.Wai.Test

data ResponseMatcher = ResponseMatcher {
  matchStatus :: Int
, matchBody :: Maybe LB.ByteString
}

instance IsString ResponseMatcher where
  fromString s = ResponseMatcher 200 (Just . encodeUtf8 . fromString $ s)

instance Num ResponseMatcher where
  fromInteger n = ResponseMatcher (fromInteger n) Nothing
  (+) =    error "ResponseMatcher does not support (+)"
  (-) =    error "ResponseMatcher does not support (-)"
  (*) =    error "ResponseMatcher does not support (*)"
  abs =    error "ResponseMatcher does not support `abs`"
  signum = error "ResponseMatcher does not support `signum`"

match :: SResponse -> ResponseMatcher -> Maybe String
match (SResponse (Status status _) _ body) (ResponseMatcher expectedStatus expectedBody) = mconcat [
    match_ "status mismatch" status expectedStatus
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
