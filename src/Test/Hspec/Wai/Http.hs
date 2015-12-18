module Test.Hspec.Wai.Http where

import           Network.Wai (Application)
import           Network.HTTP.Types

import           Test.Hspec.Core.Spec hiding (pending, pendingWith)
import           Test.Hspec.Core.Hooks

import           Test.Hspec.Wai.Internal
import           Test.Hspec.Wai.Server

import           Network.HTTP.Client

import qualified Blaze.ByteString.Builder as B

import           Data.Default.Class (def)

import           Network.Wai.Test (SResponse(..))

withServer :: IO Application -> SpecWith RequestAction -> Spec
withServer app = around $ \e -> do
  withApplication app $ \port_ -> do
    let r :: RequestAction
        r method_ path_ headers_ body_ = foo
          where
            (segments, query_) = decodePath path_

            foo :: IO SResponse
            foo = do
              manager <- newManager defaultManagerSettings
              let request = def {
                    port = port_
                  , requestHeaders = headers_
                  , path = B.toByteString (encodePathSegments segments)
                  , queryString = renderQuery True query_
                  , method = method_
                  , redirectCount = 0
                  , responseTimeout = Nothing
                  , checkStatus = \_ _ _ -> Nothing
                  , requestBody = RequestBodyLBS body_
                  }
              response <- httpLbs request manager
              return $ SResponse {
                  simpleStatus = responseStatus response
                , simpleHeaders = responseHeaders response
                , simpleBody = responseBody response
                }
    e r
