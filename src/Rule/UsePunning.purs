module Rule.UsePunning (rule) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (snd)
import PureScript.CST.Types (Expr(..), Ident(..), Label(..), Name(..), QualifiedName(..), RecordLabeled(..), Separated(..), Wrapped(..))
import Rule (Issue, expressionIssueIdentifier)
import Rule as Rule

rule :: Rule.Rule
rule = Rule.mkWithNoConfig
  { name: "UsePunning"
  , description: "Punning is easier to read."
  , examples:
      { failingCode:
          [ "x = { a: a, b: b }"
          , "x = { a: 10, b: b }"
          , "x = f { a: a, b: b }"
          ]
      , passingCode:
          [ "x = { a: 10, b: false }"
          , "x = { a: 10 }"
          , "x = { a: SomeModule.a }"
          ]
      }
  , moduleIssueIdentifier: const $ expressionIssueIdentifier $ case _ of
      ExprRecord (Wrapped { value: Just (Separated { head, tail }) }) ->
        couldBePun =<< Array.cons head (tail <#> snd)
      _ -> []
  }

couldBePun :: forall e. RecordLabeled (Expr e) -> Array Issue
couldBePun (RecordField (Name { name: Label name }) { range } (ExprIdent (QualifiedName { module: Nothing, name: Ident (identifierName) }))) =
  guard (name == identifierName) $ pure { message: "Use punning", sourceRange: range }
couldBePun _ = []
