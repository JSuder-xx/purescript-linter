module Linter.ModuleRules.Formatting.WhereClause where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Linter.ModuleRule (Issue, RuleCategory(..), declarationIssueIdentifierInModule)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Range (rangeOf)
import PureScript.CST.SourceRange (isAbove, leftAligned)
import PureScript.CST.Types (Declaration(..), Guarded(..), LetBinding(..), SourceRange, Where(..))

whereLeftAligned :: ModuleRule.ModuleRule
whereLeftAligned = ModuleRule.mkWithNoConfig
  { name: "WhereClause.LeftAligned"
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
          , """
x =
  y 10 + 1
  where
  y a =
    z 20 + 30
    where q = 1
          """
          , """
x =
  y 10 + 1
  where
  y a =
    z 20 + 30
    where
    q =
      abc
      where abc = 20
          """
          ]
      , passingCode:
          [ """
x =
  y + 1
  where
  y = 20
          """
          , """
x =
  y 10 + 1
  where
  y a =
    z 20 + 30
    where
    q = 1
          """
          , """
x =
  y 10 + 1
  where
  y a =
    z 20 + 30
    where
    q =
      abc
      where
      abc = 20
          """
          ]
      }
  , moduleIssueIdentifier: const $ declarationIssueIdentifierInModule $ case _ of
      DeclValue { guarded: Unconditional _ whereClause } -> checkWhere whereClause
      _ -> []
  }
  where
  checkWhere :: Where Void -> Array Issue
  checkWhere (Where { bindings: Nothing }) = []
  checkWhere (Where { expr, bindings: Just (Tuple { range: whereTokenRange } bindings) }) =
    let
      exprRange = rangeOf expr
    in
      fold
        [ -- the where token needs to be on the next line under the
          guard (not $ exprRange `isAbove` whereTokenRange) [ { message: "The `where` needs to be below the expression", sourceRange: whereTokenRange } ]
        , -- all the bindings need to left align under the where token
          bindings # NonEmptyArray.toArray >>= checkBinding whereTokenRange
        ]

  checkBinding :: SourceRange -> LetBinding Void -> Array Issue
  checkBinding whereTokenRange letBinding =
    fold
      [ guard (not (whereTokenRange `leftAligned` bindingRange))
          [ { message: "The binding under `where` should be left aligned with the introducing `where`.", sourceRange: bindingRange } ]
      , case letBinding of
          LetBindingName { guarded: Unconditional _ whereClause } -> checkWhere whereClause
          LetBindingName { guarded: Guarded _ } -> []
          LetBindingPattern _ _ whereClause -> checkWhere whereClause
          LetBindingSignature _ -> []
          LetBindingError _ -> []
      ]
    where
    bindingRange = rangeOf letBinding
