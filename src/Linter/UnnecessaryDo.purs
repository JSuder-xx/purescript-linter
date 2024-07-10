module Linter.UnnecessaryDo (linter) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.List (List(..), (:))
import Data.List as List
import Data.Monoid (guard)
import Linter (expressionLintProducer)
import Linter as Linter
import PureScript.CST.Types (DoStatement(..), Expr(..), Ident(..), QualifiedName(..))

linter :: Linter.Linter
linter =
  { name: "UnnecessaryDo"
  , examples:
      { bad:
          [ "x = do\n  pure 10"
          , "x = do\n  x <- thing\n  pure x"
          ]
      , good:
          [ "x = do\n  x <- thing\n  y x"
          , "x = do\n  y <- thing\n  z <- otherThing\n  pure $ y + z"
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprDo { keyword, statements } ->
        guard (NonEmptyArray.length statements == 1)
          [ { message: "Unnecessary `do` keyword. When there is only a single line in the `do` block then this can be removed.", sourceRange: keyword.range } ]
          <> guard (singleBindFollowedByPure statements)
            [ { message: "Unnecessary `do` block. A single monadic bind followed immediately by a `pure` is the Functor `map` operation by definition.", sourceRange: keyword.range } ]
      _ -> []
  }

singleBindFollowedByPure :: forall e. NonEmptyArray (DoStatement e) -> Boolean
singleBindFollowedByPure = List.fromFoldable >>> case _ of
  (DoBind _ _ _) : (DoDiscard (ExprApp (ExprIdent (QualifiedName { name: Ident "pure" })) _)) : Nil -> true
  _ -> false
