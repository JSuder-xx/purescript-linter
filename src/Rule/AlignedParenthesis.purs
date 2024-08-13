module Rule.AlignedParenthesis where

import Prelude

import Data.Monoid (guard)
import Rule (LintResults, allExpressionsLintProducer, typeLintProducer)
import Rule as Rule
import PureScript.CST.Expr as Expr
import PureScript.CST.Types (Type(..), Wrapped(..))

rule :: Rule.Rule
rule = Rule.mkWithNoConfig
  { name: "AlignedParenthesis"
  , examples:
      { bad:
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
      , good:
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
  , lintProducer: (allExpressionsLintProducer $ Expr.allParenthesis >=> parens)
      <>
        ( typeLintProducer $ case _ of
            TypeParens x -> parens x
            TypeRow x -> parens x
            _ -> []
        )
  }
  where
  parens :: forall a. Wrapped a -> LintResults
  parens (Wrapped { open: { range: open }, close: { range: close } }) =
    guard (open.start.column /= close.start.column && close.start.line > open.start.line)
      [ { message: "Parenthesis must be aligned in the same column when they appear on different lines.", sourceRange: close } ]

