{-# LANGUAGE FlexibleInstances #-}
module Test.Hspec.Wai.QuickCheck (
  property
, (==>)

-- * Re-exports
, Arbitrary (..)
, module Test.QuickCheck.Gen

-- * Internals
, Testable (..)
, WaiProperty (..)
) where

import           Test.QuickCheck hiding (Testable, property, (==>))
import qualified Test.QuickCheck as QuickCheck
import           Test.QuickCheck.Gen
import           Network.Wai (Application)

import           Test.Hspec.Wai.Internal

property :: Testable a => a -> Application -> Property
property = unWaiProperty . toProperty

data WaiProperty = WaiProperty {unWaiProperty :: Application -> Property}

class Testable a where
  toProperty :: a -> WaiProperty

instance Testable WaiProperty where
  toProperty = id

instance Testable WaiExpectation where
  toProperty action = WaiProperty (QuickCheck.property . runWaiSession action)

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  toProperty prop = WaiProperty $ QuickCheck.property . (flip $ unWaiProperty . toProperty . prop)

infixr 0 ==>
(==>) :: Testable prop => Bool -> prop -> WaiProperty
(==>) = lift (QuickCheck.==>)

lift :: Testable prop => (a -> Property -> Property) -> a -> prop -> WaiProperty
lift f a prop = WaiProperty $ \app -> f a (unWaiProperty (toProperty prop) app)
