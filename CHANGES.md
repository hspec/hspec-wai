## Changes in 0.11.0
  - Add support for cookies

## Changes in 0.10.1
  - Fix the build on GHC <7.10

## Changes in 0.10.0
  - Add support for custom state

## Changes in 0.9.2
  - Add `MonadFail` for `WaiSession` for older versions of base

## Changes in 0.9.1
  - Add `MonadFail` for `WaiSession` (GHC 8.6.1+)

## Changes in 0.9.0
  - Ignore `charset=utf-8` when matching JSON
    (as per http://www.iana.org/assignments/media-types/application/json)

## Changes in 0.8.0
  - Reject header `application/json; charset=utf-8` when matching JSON
    (as per http://www.iana.org/assignments/media-types/application/json)

## Changes in 0.7.0
  - Add proper support for GHC 8 source locations

## Changes in 0.6.6
  - Depend on `with-location`

## Changes in 0.6.5
  - Add proper support for source locations

## Changes in 0.6.4
  - Add support for QuickCheck
  - Compare headers case-insensitive

## Changes in 0.6.3
  - Add `options` to `Test.Hspec.Wai`

## Changes in 0.6.2
  - Add `withApplication` to `Test.Hspec.Wai.Internal`

## Changes in 0.6.1
  - Add `postHtmlForm` to `Test.Hspec.Wai`
