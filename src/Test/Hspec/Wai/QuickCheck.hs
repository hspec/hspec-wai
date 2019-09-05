{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
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

property :: Testable a => a -> (State a, Application) -> Property
property = unWaiProperty . toProperty

data WaiProperty st = WaiProperty {unWaiProperty :: (st, Application) -> Property}

class Testable a where
  type State a
  toProperty :: a -> WaiProperty (State a)

instance Testable (WaiProperty st) where
  type State (WaiProperty st) = st
  toProperty = id

instance Testable (WaiExpectation st) where
  type State (WaiExpectation st) = st
  toProperty action = WaiProperty (QuickCheck.property . runWithState action)

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  type State (a -> prop) = State prop
  toProperty prop = WaiProperty $ QuickCheck.property . (flip $ unWaiProperty . toProperty . prop)

infixr 0 ==>
(==>) :: Testable prop => Bool -> prop -> WaiProperty (State prop)
(==>) = lift (QuickCheck.==>)

lift :: Testable prop => (a -> Property -> Property) -> a -> prop -> WaiProperty (State prop)
lift f a prop = WaiProperty $ \app -> f a (unWaiProperty (toProperty prop) app)
