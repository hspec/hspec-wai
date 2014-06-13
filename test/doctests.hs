module Main where

import           Test.DocTest

main :: IO ()
main = doctest [
    "-isrc"
  , "-hide-all-packages"
  , "-package base"
  , "-package bytestring"
  , "-package text"
  , "-package transformers"
  , "-package case-insensitive"
  , "-package http-types"
  , "-package wai"
  , "-package wai-extra"
  , "-package hspec2"
  , "-package template-haskell"
  , "-package aeson"
  , "-package aeson-qq"
  , "src/Test/Hspec/Wai/JSON.hs"
  ]
