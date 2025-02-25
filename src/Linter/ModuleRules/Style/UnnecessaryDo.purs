module Linter.ModuleRules.Style.UnnecessaryDo (rule) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.List (List(..), (:))
import Data.List as List
import Data.Monoid (guard)
import Linter.ModuleRule (RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Types (DoStatement(..), Expr(..), Ident(..), QualifiedName(..))

rule :: ModuleRule.ModuleRule
rule = ModuleRule.mkWithNoConfig
  { name: "NoUnnecessaryDo"
  , category: Style
  , description:
      """A Monadic bind followed by a pure is actually a Functor map. It is more truthful to represent this as a narrower Functor map.

A `do` with no binds and no let declarations is unnecessary. Readers should expect a sequence of monadic binds when they see the `do` keyword."""
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ "x = do\n  pure 10"
          , "x = do\n  x <- thing\n  pure x"
          ]
      , passingCode:
          [ "x = do\n  x <- thing\n  y x"
          , "x = do\n  y <- thing\n  z <- otherThing\n  pure $ y + z"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
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
