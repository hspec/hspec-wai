{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Wai.Util where

import           Control.Monad
import           Data.Maybe
import           Data.Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Types

formatHeader :: Header -> String
formatHeader header@(name, value) = fromMaybe (show header) (safeToString $ B.concat [CI.original name, ": ", value])

safeToString :: ByteString -> Maybe String
safeToString bs = do
  str <- either (const Nothing) (Just . T.unpack) (T.decodeUtf8' bs)
  let isSafe = not $ case str of
        [] -> True
        _  -> isSpace (head str) || isSpace (last str) || any (not . isPrint) str
  guard isSafe >> return str
