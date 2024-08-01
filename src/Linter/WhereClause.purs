module Linter.WhereClause where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Linter (declarationLintProducer)
import Linter as Linter
import PureScript.CST.Range (rangeOf)
import PureScript.CST.SourceRange (isAbove, leftAligned)
import PureScript.CST.Types (Declaration(..), Guarded(..), Where(..))

leftAlignedWhere :: Linter.Linter
leftAlignedWhere =
  { name: "WhereClauseLeftAligned"
  , examples:
      { bad:
          [ """
x = y + 1 where y = 20
          """
          , """
x = 
  y + 1 
  where y = 20
          """
          ]
      , good:
          [ """
x =
  y + 1
  where
  y = 20
          """
          ]
      }
  , lintProducer: declarationLintProducer $ case _ of
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

