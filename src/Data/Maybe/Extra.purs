module Data.Maybe.Extra where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as Maybe

-- | Given a function that updates/transforms an `a` but may fail, return a function that recovers from failure by returning the original.
recover :: forall a. (a -> Maybe a) -> (a -> a)
recover f a = Maybe.fromMaybe a $ f a
