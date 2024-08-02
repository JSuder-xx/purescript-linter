module Linter.UsePunning (linter) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (snd)
import Linter (LintResult, expressionLintProducer)
import Linter as Linter
import PureScript.CST.Types (Expr(..), Ident(..), Label(..), Name(..), QualifiedName(..), RecordLabeled(..), Separated(..), Wrapped(..))

linter :: Linter.Linter
linter = Linter.mkWithNoConfig
  { name: "UsePunning"
  , examples:
      { bad:
          [ "x = { a: a, b: b }"
          , "x = { a: 10, b: b }"
          ]
      , good:
          [ "x = { a: 10, b: false }"
          , "x = { a: 10 }"
          , "x = { a: SomeModule.a }"
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprRecord (Wrapped { value: Just (Separated { head, tail }) }) ->
        couldBePun =<< Array.cons head (tail <#> snd)
      _ -> []
  }

couldBePun :: forall e. RecordLabeled (Expr e) -> Array LintResult
couldBePun (RecordField (Name { name: Label name }) { range } (ExprIdent (QualifiedName { module: Nothing, name: Ident (identifierName) }))) =
  guard (name == identifierName) $ pure { message: "Use punning", sourceRange: range }
couldBePun _ = []
