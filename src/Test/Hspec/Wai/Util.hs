{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Wai.Util where

import           Data.Char
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Types

formatHeader :: Header -> String
formatHeader header@(name, value)
  | isSpace (head formatted) = fallback
  | isSpace (last formatted) = fallback
  | any (not . isPrint) formatted = fallback
  | otherwise = formatted
  where
    fallback = show header
    formatted = (either (const fallback) T.unpack . T.decodeUtf8' . B.concat) [CI.original name, ": ", value]
