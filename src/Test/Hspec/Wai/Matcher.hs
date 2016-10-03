{-# LANGUAGE ViewPatterns #-}
module Test.Hspec.Wai.Matcher (
  ResponseMatcher(..)
, MatchHeader(..)
, MatchBody(..)
, Body
, (<:>)
, bodyEquals
, bodyContains
, match
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.Maybe
import           Data.String
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T
import           Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types
import           Network.Wai.Test

import           Test.Hspec.Wai.Util

type Body = LB.ByteString

data ResponseMatcher = ResponseMatcher {
  matchStatus :: Int
, matchHeaders :: [MatchHeader]
, matchBody :: MatchBody
}

data MatchHeader = MatchHeader ([Header] -> Body -> Maybe String)

data MatchBody = MatchBody ([Header] -> Body -> Maybe String)

bodyEquals :: Body -> MatchBody
bodyEquals body = MatchBody (\_ actual -> bodyMatcher actual body)
  where
    bodyMatcher :: Body -> Body -> Maybe String
    bodyMatcher (toStrict -> actual) (toStrict -> expected) = actualExpected "body mismatch:" actual_ expected_ <$ guard (actual /= expected)
      where
        (actual_, expected_) = case (safeToString actual, safeToString expected) of
          (Just x, Just y) -> (x, y)
          _ -> (show actual, show expected)

bodyContains :: Body -> MatchBody
bodyContains sub = MatchBody $ \_ full -> bodyMismatch "body substring search failed:" full sub
                                              <$ guard (not $ toStrict sub `isInfixOf` toStrict full)

bodyMismatch :: String -> Body -> Body -> String
bodyMismatch msg actual expected = actualExpected msg actual_ expected_
  where
    f = safeToString . toStrict
    (actual_, expected_) = case (f actual, f expected) of
      (Just x, Just y) -> (x, y)
      _ -> (show actual, show expected)

matchAny :: MatchBody
matchAny = MatchBody (\_ _ -> Nothing)

instance IsString MatchBody where
  fromString = bodyEquals . encodeUtf8 . T.pack

instance IsString ResponseMatcher where
  fromString = ResponseMatcher 200 [] . fromString

instance Num ResponseMatcher where
  fromInteger n = ResponseMatcher (fromInteger n) [] matchAny
  (+) =    error "ResponseMatcher does not support (+)"
  (-) =    error "ResponseMatcher does not support (-)"
  (*) =    error "ResponseMatcher does not support (*)"
  abs =    error "ResponseMatcher does not support `abs`"
  signum = error "ResponseMatcher does not support `signum`"

match :: SResponse -> ResponseMatcher -> Maybe String
match (SResponse (Status status _) headers body) (ResponseMatcher expectedStatus expectedHeaders (MatchBody bodyMatcher)) = mconcat [
    actualExpected "status mismatch:" (show status) (show expectedStatus) <$ guard (status /= expectedStatus)
  , checkHeaders headers body expectedHeaders
  , bodyMatcher headers body
  ]

actualExpected :: String -> String -> String -> String
actualExpected message actual expected = unlines [
    message
  , "  expected: " ++ expected
  , "  but got:  " ++ actual
  ]

checkHeaders :: [Header] -> Body -> [MatchHeader] -> Maybe String
checkHeaders headers body m = case go m of
    [] -> Nothing
    xs -> Just (mconcat xs ++ "the actual headers were:\n" ++ unlines (map formatHeader headers))
  where
    go = catMaybes . map (\(MatchHeader p) -> p headers body)

(<:>) :: HeaderName -> ByteString -> MatchHeader
name <:> value = MatchHeader $ \headers _body -> guard (header `notElem` headers) >> (Just . unlines) [
    "missing header:"
  , formatHeader header
  ]
  where
    header = (name, value)
