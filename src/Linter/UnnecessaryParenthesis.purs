module Linter.UnnecessarParenthesis (linter) where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Linter (expressionLintProducer)
import Linter as Linter
import PureScript.CST.Expr as Expr
import PureScript.CST.Types (Expr(..), Operator(..), QualifiedName(..), Wrapped(..))

linter :: Linter.Linter
linter =
  { name: "UnnecessaryParenthesis"
  , examples:
      { bad:
          [ "x = (1)"
          , "x = (\"Hi\")"
          , "x = (y)"
          , "x = f $ 1"
          , "x = f $ g 23 $ 10"
          ]
      , good:
          [ "x = 1"
          , "x = (2 + 3)"
          , "x = (f y)"
          , "x = f $ g 10"
          , "x = (mempty :: Person)"
          , "x = ($)"
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprParens (Wrapped { open, value }) ->
        guard (Expr.isTerminal value) $ pure { message: "Unnecessary parenthesis around a terminal expression.", sourceRange: open.range }
      ExprOp _ ne ->
        let
          { last: Tuple (QualifiedName { token, name: (Operator operator) }) expression } = NonEmptyArray.unsnoc ne
        in
          guard (operator == "$" && Expr.isTerminal expression) $ pure { message: "Unnecessary $ preceding a terminal expression.", sourceRange: token.range }
      _ -> []
  }
