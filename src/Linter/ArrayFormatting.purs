module Linter.ArrayFormatting (linter) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Linter (expressionLintProducer)
import Linter as Linter
import PureScript.CST.Range (rangeOf)
import PureScript.CST.SourcePos (columnDifference)
import PureScript.CST.SourceRange (noSpaceBetween, spaceBetween)
import PureScript.CST.Types (Expr(..), Separated(..), SourceRange, SourceToken, Wrapped(..))

linter :: Linter.Linter
linter =
  { name: "ArrayFormatting"
  , examples:
      { bad:
          [ "x = [ ]"
          , "x = [    ]"
          , "x = [1]"
          , "x = [1, 2]"
          , "x = [ 1,2 ]"
          , "x = [ 1, 2 , 3 ]"
          ]
      , good:
          [ "x = []"
          , "x = [ [] ]"
          , "x = [ 1 ]"
          , "x = [ 1, 2 ]"
          , "x = [ 1, 2, 3 ]"
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprArray (Wrapped { open: { range: openRange }, close: { range: closeRange }, value: Nothing }) ->
        guard ((closeRange.start `columnDifference` openRange.start) > 1) $ pure { message: "An empty array should be []", sourceRange: openRange }
      ExprArray (Wrapped { open: { range: openRange }, close: { range: closeRange }, value: Just (Separated { head, tail }) }) ->
        let
          rangeHead = rangeOf head
        in
          case Array.unsnoc tail of
            Nothing ->
              guard (not $ (openRange `spaceBetween` rangeHead) && (rangeHead `spaceBetween` closeRange)) [ { message: "Missing space between surrounding {} and contents.", sourceRange: openRange } ]

            Just { last: Tuple _ lastExpr } ->
              let
                rangeLast = rangeOf lastExpr
              in
                guard (not $ (openRange `spaceBetween` rangeHead) && (rangeLast `spaceBetween` closeRange))
                  [ { message: "Missing space between surrounding {} and contents.", sourceRange: openRange } ]
                  <>
                    ( let
                        step :: { failed :: Array SourceRange, lastExprRange :: SourceRange } -> Tuple SourceToken (Expr Void) -> { failed :: Array SourceRange, lastExprRange :: SourceRange }
                        step { failed, lastExprRange } (Tuple { range: delimitterRange } expr) =
                          let
                            thisExprRange = rangeOf expr
                          in
                            { failed:
                                if (lastExprRange `noSpaceBetween` delimitterRange) && (delimitterRange `spaceBetween` thisExprRange) then
                                  failed
                                else
                                  Array.snoc failed lastExprRange
                            , lastExprRange: thisExprRange
                            }
                      in
                        (Array.foldl step { failed: [], lastExprRange: rangeHead } tail).failed <#> { message: "Incorrect spacing between the comma and preceding expression", sourceRange: _ }
                    )

      _ -> []
  }

