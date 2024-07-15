module Linter.CompactLetBinding (linter) where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray

import Data.Monoid (guard)

import Linter (expressionLintProducer)
import Linter as Linter
import PureScript.CST.Range (rangeOf)

import PureScript.CST.SourceRange (sameLine)
import PureScript.CST.Types (DoStatement(..), Expr(..))

linter :: Linter.Linter
linter =
  { name: "CompactLetBinding"
  , examples:
      { bad:
          [ """
x =
  let
    x = 1
  in
    x + 2
          """
          , """
x =
  let x = 1
  in
    x + 2
          """
          , """
x =
  let x = 1
  in
  x + 2
          """
          , """
x = do
  let 
    x = 1
    y :: Int
    y = 2
  pure $ x + y
          """
          ]
      , good:
          [ """
x =
  let x = 1 in
  x + 2
          """
          , """
x =
  let x :: Int
      x = 1 
      y = 2 in
  x + 2
          """
          , """
x = do
  let x = 1
      y :: Int
      y = 2
  pure $ x + y
          """
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprDo { statements } -> statements # NonEmptyArray.toArray >>= case _ of
        DoLet letToken letStatements ->
          let
            ranges = rangeOf <$> letStatements
            headRange = NonEmptyArray.head ranges
          in
            guard (not $ letToken.range `sameLine` headRange) [ { message: "The first binding must be on the same line as the `let` keyword ex. `let x = 1`", sourceRange: headRange } ]
              <> (ranges # NonEmptyArray.toArray >>= \range -> guard ((letToken.range.end.column + 1) /= range.start.column) [ { message: "let bindings must align.", sourceRange: range } ])
        _ -> []
      ExprLet let' ->
        let
          headRange = rangeOf $ NonEmptyArray.head let'.bindings
          lastRange = rangeOf $ NonEmptyArray.last let'.bindings
        in
          guard (not $ let'.keyword.range `sameLine` headRange) [ { message: "The first binding must be on the same line as the `let` keyword ex. `let x = 1`", sourceRange: headRange } ]
            <> guard (not $ let'.in.range `sameLine` lastRange) [ { message: "The `in` keyword should follow the last binding on the same line", sourceRange: let'.in.range } ]

      _ -> []
  }

