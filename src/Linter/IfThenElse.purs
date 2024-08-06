module Linter.IfThenElse where

import Prelude

import Data.Monoid (guard)
import Linter (expressionLintProducer)
import Linter as Linter
import PureScript.CST.SourceRange (leftAligned, sameLine)
import PureScript.CST.Types (Expr(..))

ifThenElseLeftAligned :: Linter.Linter
ifThenElseLeftAligned = Linter.mkWithNoConfig
  { name: "IfThenElseLeftAligned"
  , examples:
      { bad:
          [ """
x = 
  if 1 == 2 
    then 3
    else 4
            """
          , """
x = 
  if 1 == 2 then 3
    else 4
          """
          , """
x = if 1 == 2 
  then 3
  else 4
          """
          ]
      , good:
          [ "x = if 1 == 2 then 3 else 4"
          , """
x = 
  if 1 == 2 then 3
  else 4
                """
          , """
x = 
  if 1 == 2 
  then 3
  else 4
                """
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprIf { keyword: { range: if' }, then: { range: then' }, else: { range: else' } } ->
        ( guard (not $ (if' `sameLine` then') || (if' `leftAligned` then'))
            $ pure { message: "When `if` and `then` are on separate lines they must be left aligned.", sourceRange: then' }
        )
          <>
            ( guard (not $ (then' `sameLine` else') || (if' `leftAligned` else'))
                $ pure { message: "When `then` and `else` are on separate lines then `else` must left align with `if`.", sourceRange: then' }
            )
      _ -> []
  }
