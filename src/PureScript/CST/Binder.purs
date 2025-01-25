module PureScript.CST.Binder where

import Prelude

import Data.Array (reverse) as Array
import Data.Array.Extra (takeMapMaybe) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import PureScript.CST.Types (Binder(..), Ident, Name)

binderVar :: forall e. Binder e -> Maybe (Name Ident)
binderVar = case _ of
  BinderVar x -> Just x
  BinderTyped b _ _ -> binderVar b
  _ -> Nothing

-- | Return a list of variable names at the end of the binder list.
lastVariables' :: forall e. NonEmptyArray (Binder e) -> Maybe (NonEmptyArray (Name Ident))
lastVariables' = NonEmptyArray.toArray >>> Array.reverse >>> Array.takeMapMaybe binderVar >>> NonEmptyArray.fromArray
