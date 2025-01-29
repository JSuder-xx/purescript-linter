module Linter.ModuleRules.AlignedParenthesis where

import Prelude

import Data.Monoid (guard)
import Linter.ModuleRule (Issue, ModuleRule, expressionIssueIdentifier, typeIssueIdentifier)
import Linter.ModuleRule as ModuleRule
import PureScript.CST.Expr as Expr
import PureScript.CST.Types (Type(..), Wrapped(..))

rule :: ModuleRule
rule = ModuleRule.mkWithNoConfig
  { name: "AlignedParenthesis"
  , description:
      "Aligning Parentheses helps the reader visually parse the two tokens."
  , examples:
      { failingCode:
          [ """
x =
  (stuff 10
      )
          """
          , """
x =
  f (stuff 10
  )
          """
          , """
x =
  f (stuff 10
        )
          """
          , """
x =
  f (stuff 10
    g 10
    h 20
  )
          """
          , """
type X = (
  x :: Int
  )
          """
          , """
type X r = ( x :: Int
  , y :: Int
  | r
  )
          """
          ]
      , passingCode:
          [ """
x =
  (stuff 10
  )
          """
          , """
x =
  f (stuff 10)
          """
          , """
x =
  f (stuff 10
    )
          """
          , """
x =
  f (stuff 10
      h 10
      h20
    )
          """
          , "type X = ( x :: Int )"
          , """
type X =
  ( x :: Int
  , y :: Int
  )
          """
          , """
type X r =
  ( x :: Int
  , y :: Int
  | r
  )
          """
          ]
      }
  , moduleIssueIdentifier: const $ (expressionIssueIdentifier $ Expr.allParenthesis >=> parens)
      <>
        ( typeIssueIdentifier $ case _ of
            TypeParens x -> parens x
            TypeRow x -> parens x
            _ -> []
        )
  }
  where
  parens :: forall a. Wrapped a -> Array Issue
  parens (Wrapped { open: { range: open }, close: { range: close } }) =
    guard (open.start.column /= close.start.column && close.start.line > open.start.line)
      [ { message: "Parenthesis must be aligned in the same column when they appear on different lines.", sourceRange: close } ]
