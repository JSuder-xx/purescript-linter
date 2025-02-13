module Linter.ModuleRules.Formatting.WhereClause where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (RuleCategory(..), declarationIssueIdentifierInModule)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Range (rangeOf)
import PureScript.CST.SourceRange (isAbove, leftAligned)
import PureScript.CST.Types (Declaration(..), Guarded(..), Where(..))

whereLeftAligned :: ModuleRule.ModuleRule
whereLeftAligned = ModuleRule.mkWithNoConfig
  { name: "WhereClauseLeftAligned"
  , category: Formatting
  , description:
      "Consistent formatting of the where clause helps readability. Left aligning the where keyword with the bindings is simply one choice."
  , examples:
      { includeModuleHeader: false
      , failingCode:
          [ """
x = y + 1 where y = 20
          """
          , """
x =
  y + 1
  where y = 20
          """
          ]
      , passingCode:
          [ """
x =
  y + 1
  where
  y = 20
          """
          ]
      }
  , moduleIssueIdentifier: const $ declarationIssueIdentifierInModule $ case _ of
      DeclValue { guarded: Unconditional _ (Where { bindings: Just (Tuple { range: whereTokenRange } bindings), expr }) } ->
        let
          exprRange = rangeOf expr
        in
          -- the where token needs to be on the next line
          guard (not $ exprRange `isAbove` whereTokenRange) [ { message: "The `where` needs to be below the expression", sourceRange: whereTokenRange } ]
            <>
              -- all the bindings need to left align under the where token
              ( ( bindings
                    # NonEmptyArray.toArray
                    <#> rangeOf
                    # Array.find (\bindingRange -> not (whereTokenRange `leftAligned` bindingRange))
                )
                  # foldMap (\badBindingRange -> [ { message: "The bindings under `where` should be left aligned with the `where`.", sourceRange: badBindingRange } ])
              )

      _ -> []
  }
