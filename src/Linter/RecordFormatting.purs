module Linter.RecordFormatting (linter) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple (Tuple(..), snd)
import Linter (expressionLintProducer)
import Linter as Linter
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.SourcePos (columnDifference)
import PureScript.CST.SourceRange (noSpaceBetween, rangeOfRecordLabeled, spaceBetween)
import PureScript.CST.Types (Expr(..), Name(..), RecordLabeled(..), Separated(..), SourceToken, Wrapped(..), SourceRange)

linter :: Linter.Linter
linter =
  { name: "RecordFormatting"
  , examples:
      { bad:
          [ "x = { }"
          , "x = {   }"
          , "x = {x: 1}"
          , "x = { x: 1}"
          , "x = {x: 1 }"
          , "x = { x :1 }"
          , "x = { x:1 }"
          , "x = { x: 1, y: 1}"
          , "x = { x: 1, y:1 }"
          , "x = { x:1, y: 1 }"
          , "x = { x: 1,y: 1 }"
          , "x = { x: 1, y: 1, z:3 }"
          , "x = { x: 1, y: 1,z: 3 }"
          ]
      , good:
          [ "x = {}"
          , "x = { x: 1 }"
          , "x = { y }"
          , "x = { x: 1, y: 2 }"
          , "x = { x: 1, y: 2, z: 3 }"
          ]
      }
  , lintProducer: expressionLintProducer $ case _ of
      ExprRecord (Wrapped { open: { range: openRange }, close: { range: closeRange }, value: Nothing }) ->
        guard ((closeRange.start `columnDifference` openRange.start) > 1) $ pure { message: "An empty record should be {}", sourceRange: openRange }
      ExprRecord (Wrapped { open: { range: openRange }, close: { range: closeRange }, value: Just (Separated { head, tail }) }) ->
        let
          rangeHead = rangeOfRecordLabeled head
        in
          case Array.unsnoc tail of
            Nothing ->
              guard (not $ (openRange `spaceBetween` rangeHead) && (rangeHead `spaceBetween` closeRange)) [ { message: "Missing space between surrounding {} and contents.", sourceRange: openRange } ]
                <> guard (not $ recordLabeledProperlySpaced head) [ { message: "Incorrect spacing in field assignment. Should be `field: value`.", sourceRange: rangeHead } ]
            Just { last: Tuple _ lastExpr } ->
              let
                rangeLast = rangeOfRecordLabeled lastExpr
                allLabeledRecords = Array.cons head $ snd <$> tail
              in
                guard (not $ (openRange `spaceBetween` rangeHead) && (rangeLast `spaceBetween` closeRange))
                  [ { message: "Missing space between surrounding {} and contents.", sourceRange: openRange } ]
                  <>
                    ( Array.find (not <<< recordLabeledProperlySpaced) allLabeledRecords
                        # foldMap \failed -> [ { message: "Incorrect spacing in field assignment. Should be `field: value`.", sourceRange: rangeOfRecordLabeled failed } ]
                    )
                  <>
                    ( let
                        step :: { failed :: Array SourceRange, lastExprRange :: SourceRange } -> Tuple SourceToken (RecordLabeled (Expr Void)) -> { failed :: Array SourceRange, lastExprRange :: SourceRange }
                        step { failed, lastExprRange } (Tuple { range: delimitterRange } expr) =
                          let
                            thisExprRange = rangeOfRecordLabeled expr
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

recordLabeledProperlySpaced :: forall e. RangeOf e => RecordLabeled (Expr e) -> Boolean
recordLabeledProperlySpaced (RecordPun _) = true
recordLabeledProperlySpaced (RecordField (Name { token: { range: nameRange } }) { range: colonRange } expr) =
  nameRange `noSpaceBetween` colonRange && colonRange `spaceBetween` (rangeOf expr)
