module Linter.ModuleRules.Style.UsePunning (rule) where

import Prelude

import Data.Array as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (snd)
import Linter.ModuleRule (Issue, RuleCategory(..), expressionIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Types (Expr(..), Ident(..), Label(..), Name(..), QualifiedName(..), RecordLabeled(..), Separated(..), Wrapped(..))

rule :: ModuleRule.ModuleRule
rule = ModuleRule.mkWithNoConfig
  { name: "UsePunning"
  , description:
      """Punning is easier to read because it reduces the noise of unnecessary repetition. By removing unnecessary repetition, true differences stand out more.

For example, can you spot the difference in `{ alice: alice, bob: bob, cindy: cindy', dave: dave }`?
Now with punning that is `{ alice, bob, cindy: cindy', dave }`."""
  , category: Style
  , examples:
      { includeModuleHeader: false
      , failingCode:
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
couldBePun (RecordField (Name { name: Label fieldName }) { range } (ExprIdent (QualifiedName { module: Nothing, name: Ident identifierName }))) =
  guard (fieldName == identifierName)
    [ { message: fold
          [ "Use record field punning to simplify `"
          , fieldName
          , ": "
          , identifierName
          , "` to just `"
          , fieldName
          , "`."
          ]
      , sourceRange: range
      }
    ]
couldBePun _ = []
