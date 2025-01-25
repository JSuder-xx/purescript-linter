module Data.Map.Extra where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.NonEmpty (NonEmpty, singleton)
import Data.Tuple (Tuple(..))

indexedBy :: forall a k. Ord k => (a -> k) -> Array a -> Map k (NonEmpty Array a)
indexedBy f = Map.fromFoldableWith (<>) <<< map (lift2 Tuple f singleton)

keyCountMap :: forall f k. Foldable f => Functor f => Ord k => f k -> Map k Int
keyCountMap f = Map.fromFoldableWith (+) $ map (lift2 Tuple identity (const 1)) f

keyCountLookup :: forall f k. Foldable f => Functor f => Ord k => f k -> k -> Int
keyCountLookup f = fromMaybe 0 <$> (flip Map.lookup $ keyCountMap f)
