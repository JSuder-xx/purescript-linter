module PureScript.CST.Separated where

import Prelude

import Data.Array as Array
import Data.Tuple (snd)
import PureScript.CST.Types (Separated(..))

values :: forall a. Separated a -> Array a
values (Separated { head, tail }) = Array.cons head $ snd <$> tail