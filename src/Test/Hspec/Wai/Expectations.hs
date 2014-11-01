module Test.Hspec.Wai.Expectations
( -- * Matching on the response
  shouldRespondWith

  -- * Re-exports from Test.Hspec
, shouldBe
, shouldSatisfy
, shouldReturn
)
where

import           Data.Foldable
import           Control.Monad.IO.Class
import           Control.Exception (Exception)
import           Network.Wai.Test hiding (request)
import qualified Test.Hspec.Expectations.Lifted as Hspec

import           Test.Hspec.Wai.Internal
import           Test.Hspec.Wai.Matcher

-- | Set the expectation that a response matches a specified `ResponseMatcher`.
--
-- A @ResponseMatcher@ matches a response if:
--
-- * the specified status matches the HTTP response status code
--
-- * the specified body (if any) matches the response body
--
-- * the response has all of the specified `Header` fields
--   (the response may have arbitrary additional `Header` fields)
--
-- You can use @ResponseMatcher@'s (broken) `Num` instance to match for a HTTP
-- status code:
--
-- > get "/" `shouldRespondWith` 200
-- > -- matches if status is 200
--
-- You can use @ResponseMatcher@'s `IsString` instance to match for a HTTP
-- status @200@ and a body:
--
-- > get "/" `shouldRespondWith` "foo"
-- > -- matches if body is "foo" and status is 200
--
-- If you want to match for a different HTTP status, you can use record update
-- notation to specify `matchStatus` explicitly:
--
-- > get "/" `shouldRespondWith` "foo" {matchStatus = 404}
-- > -- matches if body is "foo" and status is 404
--
-- If you want to require a specific header field you can specify
-- `matchHeaders`:
--
-- > get "/" `shouldRespondWith` "foo" {matchHeaders = ["Content-Type" <:> "text/plain"]}
-- > -- matches if body is "foo", status is 200 and ther is a header field "Content-Type: text/plain"
shouldRespondWith :: WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith action matcher = do
  r <- action
  forM_ (match r matcher) (liftIO . Hspec.expectationFailure)

-- | Same as `Hspec.shouldBe` but lifted to the `WaiSession`-monad.
shouldBe :: (Show a, Eq a) => a -> a -> WaiExpectation
shouldBe = Hspec.shouldBe

-- | Same as `Hspec.shouldSatisfy` but lifted to the `WaiSession`-monad.
shouldSatisfy :: Show a => a -> (a -> Bool) -> WaiExpectation
shouldSatisfy = Hspec.shouldSatisfy

-- | Same as `Hspec.shouldReturn` but lifted to the `WaiSession`-monad.
shouldReturn :: (Show a, Eq a) => WaiSession a -> a -> WaiExpectation
shouldReturn = Hspec.shouldReturn
