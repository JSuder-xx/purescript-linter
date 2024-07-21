module Data.Array.Extra where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Maybe as Maybe

-- | Take from an array while a mapping function returns `Just`. 
takeMapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
takeMapMaybe f = map f >>> Array.takeWhile Maybe.isJust >>> Array.catMaybes
