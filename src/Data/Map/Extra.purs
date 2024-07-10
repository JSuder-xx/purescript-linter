module Data.Map.Extra where

import Prelude

import Control.Apply (lift2)
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty (NonEmpty, singleton)
import Data.Tuple (Tuple(..))

indexedBy :: forall a k. Ord k => (a -> k) -> Array a -> Map k (NonEmpty Array a)
indexedBy f = Map.fromFoldableWith (<>) <<< map (lift2 Tuple f singleton)